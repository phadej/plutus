{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module PlutusTx.Test (
    -- * Size tests
    fitsInto,
    fitsUnder,
    -- * Compilation testing
    goldenPir,
    goldenTPlc,
    goldenUPlc,
    -- * Evaluation testing
    goldenEvalCek,
    goldenEvalCekLog,
    -- * Budget testing
    goldenBudget
    ) where

import Prelude

import Control.Exception
import Control.Lens (_1, view)
import Control.Monad.Except
import Control.Monad.Reader qualified as Reader
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Flat (Flat)
import Prettyprinter
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Extras
import Test.Tasty.Providers (IsTest (run, testOptions), singleTest, testFailed, testPassed)
import Type.Reflection (Typeable)

import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import PlutusCore.Pretty
import PlutusCore.Test
import PlutusTx.Code (CompiledCode, CompiledCodeIn, getPir, getPlc, sizePlc)
import PlutusTx.Evaluation
import PlutusTx.Evaluation qualified as PlutusTx
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC


-- Size testing for Tasty

fitsInto :: forall (a :: Type) .
  (Typeable a) =>
  String -> CompiledCode a -> Integer -> TestTree
fitsInto name cc = singleTest name . SizeTest cc

data SizeTest (a :: Type) = SizeTest (CompiledCode a) Integer

instance (Typeable a) => IsTest (SizeTest a) where
  run _ (SizeTest cc limit) _ = do
    let estimate = sizePlc cc
    let diff = limit - estimate
    pure $ case signum diff of
      (-1) -> testFailed $ "Exceeded limit by " <> (show . abs $ diff)
      0    -> testPassed $ "AST size: " <> show limit
      _    -> testPassed $ "Remaining headroom: " <> show diff
  testOptions = Tagged []

fitsUnder :: forall (a :: Type) .
  (Typeable a) =>
  String -> (String, CompiledCode a) -> (String, CompiledCode a) -> TestTree
fitsUnder name test target = singleTest name $ SizeComparisonTest test target

data SizeComparisonTest (a :: Type) =
  SizeComparisonTest (String, CompiledCode a) (String, CompiledCode a)

instance (Typeable a) => IsTest (SizeComparisonTest a) where
  run _ (SizeComparisonTest (mName, mCode) (tName, tCode)) _ = do
    let tEstimate = sizePlc tCode
    let mEstimate = sizePlc mCode
    let diff = tEstimate - mEstimate
    pure $ case signum diff of
      (-1) -> testFailed . renderFailed (tName, tEstimate) (mName, mEstimate) $ diff
      0    -> testPassed . renderEstimates (tName, tEstimate) $ (mName, mEstimate)
      _    -> testPassed . renderExcess (tName, tEstimate) (mName, mEstimate) $ diff
  testOptions = Tagged []

renderFailed :: (String, Integer) -> (String, Integer) -> Integer -> String
renderFailed tData mData diff = renderEstimates tData mData <>
                                "Exceeded by: " <> show diff

renderEstimates :: (String, Integer) -> (String, Integer) -> String
renderEstimates (tName, tEstimate) (mName, mEstimate) =
  "Target: " <> tName <> "; size " <> show tEstimate <> "\n" <>
  "Measured: " <> mName <> "; size " <> show mEstimate <> "\n"

renderExcess :: (String, Integer) -> (String, Integer) -> Integer -> String
renderExcess tData mData diff = renderEstimates tData mData <>
                                "Remaining headroom: " <> show diff

-- Budget testing

goldenBudget :: TestName -> CompiledCode a -> TestNested
goldenBudget name compiledCode = do
  path <- Reader.ask
  let budgetText = measureBudget compiledCode
      filename = foldr (</>) (name ++ ".budget.golden") path
  pure $
    goldenVsDoc name filename (maybe "Failed" pretty budgetText)

-- TODO: expose something like this somewhere more useful
measureBudget :: CompiledCode a -> Maybe PLC.ExBudget
measureBudget compiledCode =
  let programE = PLC.runQuote
               $ runExceptT @PLC.FreeVariableError
               $ UPLC.unDeBruijnProgram
               $ getPlc compiledCode
   in case programE of
        Left _ -> Nothing
        Right program ->
          let (_, UPLC.TallyingSt _ budget, _) = PlutusTx.evaluateCekTrace program
           in Just budget

-- Compilation testing

goldenPir
    :: (PLC.Closed uni, uni `PLC.Everywhere` PrettyConst, uni `PLC.Everywhere` Flat, PLC.GShow uni, Pretty fun, Flat fun)
    => String -> CompiledCodeIn uni fun a -> TestNested
goldenPir name value = nestedGoldenVsDoc name $ pretty $ getPir value

-- Evaluation testing

-- TODO: rationalize with the fucntions exported from PlcTestUtils
goldenEvalCek :: ToUPlc a PLC.DefaultUni PLC.DefaultFun => String -> [a] -> TestNested
goldenEvalCek name values = nestedGoldenVsDocM name $ prettyPlcClassicDebug <$> (rethrow $ runPlcCek values)

goldenEvalCekLog :: ToUPlc a PLC.DefaultUni PLC.DefaultFun => String -> [a] -> TestNested
goldenEvalCekLog name values = nestedGoldenVsDocM name $ pretty . view _1 <$> (rethrow $ runPlcCekTrace values)

-- Helpers

instance (PLC.Closed uni, uni `PLC.Everywhere` Flat, uni `PLC.Everywhere` PrettyConst, PLC.GShow uni, Pretty fun, Flat fun) =>
            ToUPlc (CompiledCodeIn uni fun a) uni fun where
    toUPlc v = do
        v' <- catchAll $ getPlc v
        toUPlc v'

runPlcCek :: ToUPlc a PLC.DefaultUni PLC.DefaultFun => [a] -> ExceptT SomeException IO (UPLC.Term PLC.Name PLC.DefaultUni PLC.DefaultFun ())
runPlcCek values = do
     ps <- traverse toUPlc values
     let p = foldl1 UPLC.applyProgram ps
     either (throwError . SomeException) pure $ evaluateCek p

runPlcCekTrace ::
     ToUPlc a PLC.DefaultUni PLC.DefaultFun =>
     [a] ->
     ExceptT SomeException IO ([Text], CekExTally PLC.DefaultFun, UPLC.Term PLC.Name PLC.DefaultUni PLC.DefaultFun ())
runPlcCekTrace values = do
     ps <- traverse toUPlc values
     let p = foldl1 UPLC.applyProgram ps
     let (logOut, TallyingSt tally _, result) = evaluateCekTrace p
     res <- either (throwError . SomeException) pure result
     pure (logOut, tally, res)


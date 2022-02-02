{-# LANGUAGE TypeFamilies #-}
module DeBruijn.Spec (test_debruijn) where

import Common
import DeBruijn.FlatNatWord (test_flatNatWord)
import DeBruijn.Scope (test_scope)
import DeBruijn.UnDeBruijnify (test_undebruijnify)
import Test.Tasty

test_debruijn :: TestTree
test_debruijn = runTestNestedIn ["untyped-plutus-core","test"] $
               testNested "DeBruijn"
                [ test_undebruijnify
                , test_scope
                , test_flatNatWord
                ]

module DeBruijn.FlatNatWord (test_flatNatWord) where

import Common
import Flat
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural
import Test.Tasty.Hedgehog

{- NOTE: [DeBruijjn index flat format]
In the debruijn branch we are transitioning the debruijn index to be a Word instead of Natural, for CEK performance reasons.

This means the types of `Script`,`Program`,`Term` change and so does the flat "interface".

This module tests that although the interface/types change,
the underlying flat encoding/decoding remains the same,
so there is no need to issue a new plutus language version increase.
-}

test_flatNatWord :: TestNested
test_flatNatWord = testNested "FlatNatWord"
    [ pure $ testProperty "natEncByteString = wordEncByteString" prop_Enc
    , pure $ testProperty "roundtripWord" prop_TripWord
    , pure $ testProperty "roundtripNat" prop_TripNat
    ]

prop_Enc :: Property
prop_Enc = property $ do
    w <- forAll $ Gen.word64 Range.linearBounded
    let n = fromIntegral w :: Natural
    flat w === flat n

prop_TripWord :: Property
prop_TripWord = property $ do
    w <- forAll $ Gen.word64 Range.linearBounded
    let n = fromIntegral w :: Natural
        nFlat = flat n
    case unflat nFlat of
        Right wRound -> wRound === w
        _            -> failure

prop_TripNat :: Property
prop_TripNat = property $ do
    w <- forAll $ Gen.word64 Range.linearBounded
    let n = fromIntegral w :: Natural
        wFlat = flat w
    case unflat wFlat of
        Right nRound -> nRound === n
        _            -> failure


{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Convert (props) where

import Data.Complex (Complex)
import Data.Fixed (E12, Fixed)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio)
import Gens qualified
import Hedgehog (Gen, PropertyName, (===))
import Hedgehog qualified as H
import Numeric.Convert.Internal
  ( FromInteger (fromZ),
    FromRational (fromQ),
    FromReal (fromR),
    ToInteger (toZ),
    ToRational (toQ),
    ToReal (toR),
  )
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Convert"
    [ fromIntegerProps,
      toIntegerProps,
      fromRationalProps,
      toRationalProps,
      fromRealProps,
      toRealProps
    ]

fromIntegerProps :: TestTree
fromIntegerProps =
  T.testGroup
    "FromInteger"
    [ testFromInteger @Float "Float" "floatFromInteger",
      testFromInteger @Double "Double" "doubleFromInteger",
      testFromInteger @Int "Int" "intFromInteger",
      testFromInteger @Int8 "Int8" "int8FromInteger",
      testFromInteger @Int16 "Int16" "int16FromInteger",
      testFromInteger @Int32 "Int32" "int32FromInteger",
      testFromInteger @Int64 "Int64" "int64FromInteger",
      testFromInteger @Integer "Integer" "integerFromInteger",
      testFromInteger @Word "Word" "wordFromInteger",
      testFromInteger @Word8 "Word8" "word8FromInteger",
      testFromInteger @Word16 "Word16" "word16FromInteger",
      testFromInteger @Word32 "Word32" "word32FromInteger",
      testFromInteger @Word64 "Word64" "word64FromInteger",
      testFromInteger @Natural "Natural" "naturalFromInteger",
      testFromInteger @(Ratio Integer) "Ratio Integer" "ratioIntegerFromInteger",
      testFromInteger @(Ratio Natural) "Ratio Natural" "ratioNaturalFromInteger",
      testFromInteger @(Complex Double) "Complex" "complexFromInteger",
      testFromInteger @(Fixed E12) "Fixed" "fixedFromInteger"
    ]

toIntegerProps :: TestTree
toIntegerProps =
  T.testGroup
    "ToInteger"
    [ testToInteger Gens.int "Int" "intToInteger",
      testToInteger Gens.int8 "Int8" "int8ToInteger",
      testToInteger Gens.int16 "Int16" "int16ToInteger",
      testToInteger Gens.int32 "Int32" "int32ToInteger",
      testToInteger Gens.int64 "Int64" "int64ToInteger",
      testToInteger Gens.integer "Integer" "integerToInteger",
      testToInteger Gens.word "Word" "wordToInteger",
      testToInteger Gens.word8 "Word8" "word8ToInteger",
      testToInteger Gens.word16 "Word16" "word16ToInteger",
      testToInteger Gens.word32 "Word32" "word32ToInteger",
      testToInteger Gens.word64 "Word64" "word64ToInteger",
      testToInteger Gens.natural "Natural" "naturalToInteger"
    ]

fromRationalProps :: TestTree
fromRationalProps =
  T.testGroup
    "FromRational"
    [ testFromRational @Float "Float" "floatFromRational",
      testFromRational @Double "Double" "doubleFromRational",
      testFromRational @(Ratio Integer) "Ratio Integer" "ratioIntegerFromRational",
      testFromRational @(Ratio Natural) "Ratio Natural" "ratioNaturalFromRational",
      testFromRational @(Complex Double) "Complex" "complexFromRational",
      testFromRational @(Fixed E12) "Fixed" "fixedFromRational"
    ]

toRationalProps :: TestTree
toRationalProps =
  T.testGroup
    "ToRational"
    [ testToRational Gens.float "Float" "floatToRational",
      testToRational Gens.double "Double" "doubleToRational",
      testToRational Gens.int "Int" "intToRational",
      testToRational Gens.int8 "Int8" "int8ToRational",
      testToRational Gens.int16 "Int16" "int16ToRational",
      testToRational Gens.int32 "Int32" "int32ToRational",
      testToRational Gens.int64 "Int64" "int64ToRational",
      testToRational Gens.integer "Integer" "integerToRational",
      testToRational Gens.word "Word" "wordToRational",
      testToRational Gens.word8 "Word8" "word8ToRational",
      testToRational Gens.word16 "Word16" "word16ToRational",
      testToRational Gens.word32 "Word32" "word32ToRational",
      testToRational Gens.word64 "Word64" "word64ToRational",
      testToRational Gens.natural "Natural" "naturalToRational",
      testToRational Gens.rational "Ratio Integer" "ratioIntegerToRational",
      testToRational Gens.rationalNat "Ratio Natural" "ratioNaturalToRational",
      testToRational Gens.fixed "Fixed" "fixedToRational"
    ]

fromRealProps :: TestTree
fromRealProps =
  T.testGroup
    "FromReal"
    [ testFromReal @Float "Float" "floatFromReal",
      testFromReal @Double "Double" "doubleFromReal",
      testFromReal @(Ratio Integer) "Ratio Integer" "ratioIntegerFromReal",
      testFromReal @(Ratio Natural) "Ratio Natural" "ratioNaturalFromReal",
      testFromReal @(Complex Double) "Complex" "complexFromReal",
      testFromReal @(Fixed E12) "Fixed" "fixedFromReal"
    ]

toRealProps :: TestTree
toRealProps =
  T.testGroup
    "ToReal"
    [ testToReal Gens.float "Float" "floatToReal",
      testToReal Gens.double "Double" "doubleToReal",
      testToReal Gens.int "Int" "intToReal",
      testToReal Gens.int8 "Int8" "int8ToReal",
      testToReal Gens.int16 "Int16" "int16ToReal",
      testToReal Gens.int32 "Int32" "int32ToReal",
      testToReal Gens.int64 "Int64" "int64ToReal",
      testToReal Gens.integer "Integer" "integerToReal",
      testToReal Gens.word "Word" "wordToReal",
      testToReal Gens.word8 "Word8" "word8ToReal",
      testToReal Gens.word16 "Word16" "word16ToReal",
      testToReal Gens.word32 "Word32" "word32ToReal",
      testToReal Gens.word64 "Word64" "word64ToReal",
      testToReal Gens.natural "Natural" "naturalToReal",
      testToReal Gens.rational "Ratio Integer" "ratioIntegerToReal",
      testToReal Gens.rationalNat "Ratio Natural" "ratioNaturalToReal",
      testToReal Gens.fixed "Fixed" "fixedToReal"
    ]

testFromInteger ::
  forall a.
  ( Eq a,
    FromInteger a,
    Num a,
    Show a
  ) =>
  TestName ->
  PropertyName ->
  TestTree
testFromInteger = testFromX @_ @a fromInteger fromZ Gens.integer

testToInteger ::
  forall a.
  ( Integral a,
    ToInteger a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testToInteger = testToX toInteger toZ

testFromRational ::
  forall a.
  ( Eq a,
    Fractional a,
    FromRational a,
    Show a
  ) =>
  TestName ->
  PropertyName ->
  TestTree
testFromRational = testFromX @_ @a fromRational fromQ Gens.rational

testToRational ::
  forall a.
  ( Real a,
    ToRational a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testToRational = testToX toRational toQ

testFromReal ::
  forall a.
  ( Eq a,
    FromReal a,
    Fractional a,
    Show a
  ) =>
  TestName ->
  PropertyName ->
  TestTree
testFromReal = testFromX @_ @a realToFrac fromR Gens.double

testToReal ::
  forall a.
  ( Real a,
    ToReal a,
    Show a
  ) =>
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testToReal = testToX realToFrac toR

testFromX ::
  forall a b.
  ( Eq b,
    Show a,
    Show b
  ) =>
  (a -> b) ->
  (a -> b) ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testFromX expectedFn actualFn gen testName propName = Utils.testProp testName propName $ do
  x <- H.forAll gen
  let expected = expectedFn x
      actual = actualFn x
  expected === actual

testToX ::
  forall a b.
  ( Eq b,
    Show a,
    Show b
  ) =>
  (a -> b) ->
  (a -> b) ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
testToX expectedFn actualFn gen testName propName = Utils.testProp testName propName $ do
  x <- H.forAll gen
  let expected = expectedFn x
      actual = actualFn x
  expected === actual

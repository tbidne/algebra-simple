module Test.Multiplicative.MGroup (props) where

import Equality (Equality (..))
import Gens qualified
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..), NonZero (..))
import Numeric.Algebra.Multiplicative.MGroup qualified as MGroup
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (..))
import Refined (Refined)
import Refined qualified as R
import Refined.Extras.Utils (pattern MkRefined)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

props :: TestTree
props =
  T.testGroup
    "Multiplicative Group"
    [ divProps,
      refinedDivProps,
      divIdentProps
    ]

divProps :: TestTree
divProps =
  T.testGroup
    "(.%.) === div / (/)"
    [ floatDiv,
      doubleDiv,
      intDiv,
      int8Div,
      int16Div,
      int32Div,
      int64Div,
      integerDiv,
      naturalDiv,
      wordDiv,
      word8Div,
      word16Div,
      word32Div,
      word64Div,
      rationalDiv,
      fractionDiv
    ]

floatDiv :: TestTree
floatDiv = mgroupDivEq (/) Gens.float Gens.floatNonZero (MkEqEpsilon 1.0) "Float"

doubleDiv :: TestTree
doubleDiv = mgroupDivEq (/) Gens.double Gens.doubleNonZero (MkEqEpsilon 1.0) "Double"

intDiv :: TestTree
intDiv = mgroupDivEq div Gens.int Gens.intNonZero MkEqExact "Int"

int8Div :: TestTree
int8Div = mgroupDivEq div Gens.int8 Gens.int8NonZero MkEqExact "Int8"

int16Div :: TestTree
int16Div = mgroupDivEq div Gens.int16 Gens.int16NonZero MkEqExact "Int16"

int32Div :: TestTree
int32Div = mgroupDivEq div Gens.int32 Gens.int32NonZero MkEqExact "Int32"

int64Div :: TestTree
int64Div = mgroupDivEq div Gens.int64 Gens.int64NonZero MkEqExact "Int64"

integerDiv :: TestTree
integerDiv = mgroupDivEq div Gens.integer Gens.integerNonZero MkEqExact "Integer"

naturalDiv :: TestTree
naturalDiv = mgroupDivEq div Gens.natural Gens.naturalNonZero MkEqExact "Natural"

wordDiv :: TestTree
wordDiv = mgroupDivEq div Gens.word Gens.wordNonZero MkEqExact "Word"

word8Div :: TestTree
word8Div = mgroupDivEq div Gens.word8 Gens.word8NonZero MkEqExact "Word8"

word16Div :: TestTree
word16Div = mgroupDivEq div Gens.word16 Gens.word16NonZero MkEqExact "Word16"

word32Div :: TestTree
word32Div = mgroupDivEq div Gens.word32 Gens.word32NonZero MkEqExact "Word32"

word64Div :: TestTree
word64Div = mgroupDivEq div Gens.word64 Gens.word64NonZero MkEqExact "Word64"

rationalDiv :: TestTree
rationalDiv = mgroupDivEq (/) Gens.rational Gens.rationalNonZero MkEqRatio "Rational"

fractionDiv :: TestTree
fractionDiv = mgroupDivEq (/) Gens.fraction Gens.fractionNonZero MkEqExact "Fraction"

refinedDivProps :: TestTree
refinedDivProps =
  T.testGroup
    "Refined (.%.) === base (.%.) and preserves refinement"
    [ refinedNonNegativeDiv
    ]

refinedNonNegativeDiv :: TestTree
refinedNonNegativeDiv = mgroupRefinedDivEq refToBaseNZ Gens.refinedNonNegative gen "Refined NonNegative"
  where
    refToBaseNZ = MGroup.unsafeAMonoidNonZero . R.unrefine
    gen = Gens.refinedAddNZ Gens.refinedNonNegative

divIdentProps :: TestTree
divIdentProps =
  T.testGroup
    "Division is the inverse: one == x .%. x"
    [ intDivIdent,
      int8DivIdent,
      int16DivIdent,
      int32DivIdent,
      int64DivIdent,
      integerDivIdent,
      naturalDivIdent,
      wordDivIdent,
      word8DivIdent,
      word16DivIdent,
      word32DivIdent,
      word64DivIdent,
      rationalDivIdent,
      fractionDivIdent,
      refinedNonNegativeDivIdent
    ]

intDivIdent :: TestTree
intDivIdent = agroupDivIdent Gens.intNonZero MkEqExact "Int"

int8DivIdent :: TestTree
int8DivIdent = agroupDivIdent Gens.int8NonZero MkEqExact "Int8"

int16DivIdent :: TestTree
int16DivIdent = agroupDivIdent Gens.int16NonZero MkEqExact "Int16"

int32DivIdent :: TestTree
int32DivIdent = agroupDivIdent Gens.int32NonZero MkEqExact "Int32"

int64DivIdent :: TestTree
int64DivIdent = agroupDivIdent Gens.int64NonZero MkEqExact "Int64"

integerDivIdent :: TestTree
integerDivIdent = agroupDivIdent Gens.integerNonZero MkEqExact "Integer"

naturalDivIdent :: TestTree
naturalDivIdent = agroupDivIdent Gens.naturalNonZero MkEqExact "Natural"

wordDivIdent :: TestTree
wordDivIdent = agroupDivIdent Gens.wordNonZero MkEqExact "Word"

word8DivIdent :: TestTree
word8DivIdent = agroupDivIdent Gens.word8NonZero MkEqExact "Word8"

word16DivIdent :: TestTree
word16DivIdent = agroupDivIdent Gens.word16NonZero MkEqExact "Word16"

word32DivIdent :: TestTree
word32DivIdent = agroupDivIdent Gens.word32NonZero MkEqExact "Word32"

word64DivIdent :: TestTree
word64DivIdent = agroupDivIdent Gens.word64NonZero MkEqExact "Word64"

rationalDivIdent :: TestTree
rationalDivIdent = agroupDivIdent Gens.rationalNonZero MkEqRatio "Rational"

fractionDivIdent :: TestTree
fractionDivIdent = agroupDivIdent Gens.fractionNonZero MkEqExact "Fraction"

refinedNonNegativeDivIdent :: TestTree
refinedNonNegativeDivIdent = agroupRefinedDivIdent R.andLeft gen "Refined NonNegative"
  where
    gen = Gens.refinedAddNZ Gens.refinedNonNegative

mgroupDivEq ::
  (MGroup a, NZ a ~ NonZero a, Show a) =>
  (a -> a -> a) ->
  Gen a ->
  Gen (NonZero a) ->
  (a -> Equality eq a) ->
  TestName ->
  TestTree
mgroupDivEq expectedFn gen genNZ eqCons desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        nz@(MkNonZero d) <- H.forAll genNZ
        let actual = x .%. nz
            expected = expectedFn x d
        eqCons expected === eqCons actual

mgroupRefinedDivEq ::
  ( MGroup a,
    MGroup (Refined p a),
    NZ (Refined p a) ~ nz,
    Show a,
    Show nz
  ) =>
  (nz -> NZ a) ->
  Gen (Refined p a) ->
  Gen nz ->
  TestName ->
  TestTree
mgroupRefinedDivEq dropRefNZ gen genNZ desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        rx@(MkRefined x) <- H.forAll gen
        nz <- H.forAll genNZ
        let (MkRefined actual) = rx .%. nz
            expected = x .%. dropRefNZ nz
        expected === actual

agroupDivIdent ::
  (MGroup a, NZ a ~ NonZero a, Show a) =>
  Gen (NonZero a) ->
  (a -> Equality eq a) ->
  TestName ->
  TestTree
agroupDivIdent gen eqCons desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        nz@(MkNonZero x) <- H.forAll gen
        eqCons one === eqCons (x .%. nz)

agroupRefinedDivIdent ::
  ( MGroup (Refined p a),
    NZ (Refined p a) ~ nz,
    Show a,
    Show nz
  ) =>
  (nz -> Refined p a) ->
  Gen nz ->
  TestName ->
  TestTree
agroupRefinedDivIdent nzToBase gen desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        nz <- H.forAll gen
        one === nzToBase nz .%. nz

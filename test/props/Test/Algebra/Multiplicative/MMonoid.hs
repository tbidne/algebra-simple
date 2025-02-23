{-# LANGUAGE CPP #-}

-- see NOTE: [Pattern Synonym COMPLETE]
#if !MIN_VERSION_base(4, 16, 0)
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Test.Algebra.Multiplicative.MMonoid (props) where

import Equality (Equality (MkEqExact, MkEqRatio))
import Gens qualified
import Hedgehog (Gen, PropertyName, (/==), (===))
import Hedgehog qualified as H
import Numeric.Algebra.Multiplicative.MMonoid
  ( MMonoid (one),
    pattern NonOne,
    pattern One,
  )
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup ((.*.)))
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

props :: TestTree
props =
  T.testGroup
    "Multiplicative Monoid"
    [ identityProps,
      testMMonoidSynonym
    ]

identityProps :: TestTree
identityProps =
  T.testGroup
    "Identity: one .*. x == x == x .*. one"
    [ intId,
      int8Id,
      int16Id,
      int32Id,
      int64Id,
      integerId,
      wordId,
      word8Id,
      word16Id,
      word32Id,
      word64Id,
      naturalId,
      rationalId
    ]

intId :: TestTree
intId = mmonoidIdentity Gens.int MkEqExact "Int" "intId"

int8Id :: TestTree
int8Id = mmonoidIdentity Gens.int8 MkEqExact "Int8" "int8Id"

int16Id :: TestTree
int16Id = mmonoidIdentity Gens.int16 MkEqExact "Int16" "int16Id"

int32Id :: TestTree
int32Id = mmonoidIdentity Gens.int32 MkEqExact "Int32" "int32Id"

int64Id :: TestTree
int64Id = mmonoidIdentity Gens.int64 MkEqExact "Int64" "int64Id"

integerId :: TestTree
integerId = mmonoidIdentity Gens.integer MkEqExact "Integer" "integerId"

wordId :: TestTree
wordId = mmonoidIdentity Gens.word MkEqExact "Word" "wordId"

word8Id :: TestTree
word8Id = mmonoidIdentity Gens.word8 MkEqExact "Word8" "word8Id"

word16Id :: TestTree
word16Id = mmonoidIdentity Gens.word16 MkEqExact "Word16" "word16Id"

word32Id :: TestTree
word32Id = mmonoidIdentity Gens.word32 MkEqExact "Word32" "word32Id"

word64Id :: TestTree
word64Id = mmonoidIdentity Gens.word64 MkEqExact "Word64" "word64Id"

naturalId :: TestTree
naturalId = mmonoidIdentity Gens.natural MkEqExact "Natural" "naturalId"

rationalId :: TestTree
rationalId = mmonoidIdentity Gens.rational MkEqRatio "Rational" "rationalId"

mmonoidIdentity ::
  ( MMonoid a,
    Show a
  ) =>
  Gen a ->
  (a -> Equality eq a) ->
  TestName ->
  PropertyName ->
  TestTree
mmonoidIdentity = Utils.identity (.*.) one

testMMonoidSynonym :: TestTree
testMMonoidSynonym = Utils.testPropertyCompat desc "testMMonoidSynonym" $
  H.property $ do
    x <- H.forAll Gens.integer
    case x of
      One -> one === x
      NonOne y -> do
        x === y
        one /== y
  where
    desc = "MMonoid pattern synonym"

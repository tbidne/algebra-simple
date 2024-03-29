{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils
  ( -- * Equality Functions
    binaryEq,

    -- * Laws
    associativity,
    identity,

    -- * Logic
    (==>),
    (<=>),

    -- * Misc
    testPropertyCompat,
  )
where

import Equality (Equality)
import Hedgehog (Gen, Property, PropertyName, (===))
import Hedgehog qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog qualified as TH

-- | Tests that two binary functions are pointwise equal.
binaryEq ::
  (Show a) =>
  -- | The baseline function.
  (a -> a -> a) ->
  -- | The function we compare against the baseline.
  (a -> a -> a) ->
  -- | Generator the domain.
  Gen a ->
  -- | Injects the result into an 'Equality' for the comparison.
  (a -> Equality eq a) ->
  -- | Test description.
  TestName ->
  -- | Property description.
  PropertyName ->
  TestTree
binaryEq expectedFn actualFn gen equalityCons desc propName =
  testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      let actual = actualFn x y
          expected = expectedFn x y
      equalityCons expected === equalityCons actual

-- | Tests that a function is associative.
associativity ::
  (Eq a, Show a) =>
  -- | Function to test.
  (a -> a -> a) ->
  -- | Generates the domain.
  Gen a ->
  -- | Test description.
  TestName ->
  -- | Property description.
  PropertyName ->
  TestTree
associativity f gen desc propName =
  testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      z <- H.forAll gen
      let lhsPreSum = y `f` z
          lhs = x `f` lhsPreSum
          rhsPreSum = x `f` y
          rhs = rhsPreSum `f` z
      H.annotateShow lhsPreSum
      H.annotateShow lhs
      H.annotateShow rhsPreSum
      H.annotateShow rhs
      -- x `f` (y `f` z) === (x `f` y) `f` z,
      -- but with more granular logging
      lhs === rhs

-- | Tests the identity law.
--
-- @
-- f x e === x === f e x
-- @
identity ::
  (Show a) =>
  -- | Function to test.
  (a -> a -> a) ->
  -- | Identity term.
  a ->
  -- | Generates the domain.
  Gen a ->
  -- | Injects the result into an 'Equality' for the comparison.
  (a -> Equality eq a) ->
  -- | Test description.
  TestName ->
  -- | Property description.
  PropertyName ->
  TestTree
identity f ident gen eqCons desc propName =
  testPropertyCompat desc propName $
    H.property $ do
      x <- H.forAll gen
      eqCons (f x ident) === eqCons x
      eqCons (f ident x) === eqCons x

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
_ <=> _ = False

infixr 1 <=>

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TH.testPropertyNamed
#else
testPropertyCompat tn _ = TH.testProperty tn
#endif

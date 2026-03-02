{-# LANGUAGE CPP #-}

-- see NOTE: [Pattern Synonym COMPLETE]
#if !MIN_VERSION_base(4, 16, 0)
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

-- | Provides typeclass for euclidean division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MEuclidean
  ( MEuclidean (..),
    mdiv,
    mmod,
    mgcd,
    mlcm,
  )
where

import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.AMonoid
  ( AMonoid,
    pattern NonZero,
    pattern Zero,
  )
import Numeric.Algebra.Deriving (AsIntegral (MkAsIntegral))
import Numeric.Algebra.Multiplicative.MGroup (MGroup)
import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
import Numeric.Algebra.Normed (Normed (norm))

-- $setup
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))

-- | 'MGroup' equipped with "euclidean" division.
--
-- ==== __Examples:__
--
-- >>> :{
--   -- Multiplication
--   f1 :: (MEuclidean g) => g -> g
--   f1 x = x .*. x
-- :}
--
-- >>> f1 5
-- 25
--
-- >>> :{
--   -- One
--   f2 :: (MEuclidean g) => g -> g
--   f2 x = x .*. one
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Division
--   f3 :: (MEuclidean g, Num g) => g -> g
--   f3 x = x .%. 2
-- :}
--
-- >>> f3 6
-- 3
--
-- >>> :{
--   -- Subtraction
--   f4 :: (MEuclidean g, Num g) => g -> (g, g)
--   f4 x = x `mdivMod` 4
-- :}
--
-- >>> f4 9
-- (2,1)
--
--  @since 0.1
type MEuclidean :: Type -> Constraint
class (MGroup g) => MEuclidean g where
  -- | @since 0.1
  mdivMod :: g -> g -> (g, g)

-- | @since 0.1
mdiv :: (MEuclidean g) => g -> g -> g
mdiv x d = fst $ mdivMod x d
{-# INLINE mdiv #-}

-- | @since 0.1
mmod :: (MEuclidean g) => g -> g -> g
mmod x d = snd $ mdivMod x d
{-# INLINE mmod #-}

-- | @since 0.1
mgcd :: (AMonoid g, Eq g, MEuclidean g, Normed g) => g -> g -> g
mgcd x y = gcd' (norm x) (norm y)
  where
    gcd' a Zero = a
    gcd' a (NonZero b) = gcd' b (a `mmod` b)
{-# INLINE mgcd #-}

-- | @since 0.1
mlcm :: (AMonoid g, Eq g, MEuclidean g, Normed g) => g -> g -> g
mlcm Zero _ = Zero
mlcm _ Zero = Zero
mlcm x y = norm (x `mdiv` mgcd x y .*. y)
{-# INLINE mlcm #-}

-- | @since 0.1
instance (Integral a) => MEuclidean (AsIntegral a) where
  mdivMod =
    coerce
      @(a -> a -> (a, a))
      @(AsIntegral a -> AsIntegral a -> (AsIntegral a, AsIntegral a))
      divMod
  {-# INLINE mdivMod #-}

-- | @since 0.1
deriving via (AsIntegral Int) instance MEuclidean Int

-- | @since 0.1
deriving via (AsIntegral Int8) instance MEuclidean Int8

-- | @since 0.1
deriving via (AsIntegral Int16) instance MEuclidean Int16

-- | @since 0.1
deriving via (AsIntegral Int32) instance MEuclidean Int32

-- | @since 0.1
deriving via (AsIntegral Int64) instance MEuclidean Int64

-- | @since 0.1
deriving via (AsIntegral Integer) instance MEuclidean Integer

-- | @since 0.1
deriving via (AsIntegral Word) instance MEuclidean Word

-- | @since 0.1
deriving via (AsIntegral Word8) instance MEuclidean Word8

-- | @since 0.1
deriving via (AsIntegral Word16) instance MEuclidean Word16

-- | @since 0.1
deriving via (AsIntegral Word32) instance MEuclidean Word32

-- | @since 0.1
deriving via (AsIntegral Word64) instance MEuclidean Word64

-- | @since 0.1
deriving via (AsIntegral Natural) instance MEuclidean Natural

-- | Provides typeclasses for division.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MGroup
  ( MGroup (..),
    mnegate,
  )
where

import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import GHC.Real (Ratio)
import Numeric.Algebra.Deriving
  ( AsFractional (MkAsFractional),
    AsIntegral (MkAsIntegral),
  )
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))

-- $setup
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a multiplicative group.
--
-- ==== __Examples:__
--
-- >>> :{
--   -- Multiplication
--   f1 :: (MGroup g) => g -> g
--   f1 x = x .*. x
-- :}
--
-- >>> f1 5
-- 25
--
-- >>> :{
--   -- One
--   f2 :: (MGroup g) => g -> g
--   f2 x = x .*. one
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Division
--   f3 :: (MGroup g, Num g) => g -> g
--   f3 x = x .%. 2
-- :}
--
-- >>> f3 6
-- 3
--
-- @since 0.1
type MGroup :: Type -> Constraint
class (MMonoid g) => MGroup g where
  -- | Should satisfy:
  --
  -- @
  -- -- inverse
  -- a .%. a === one
  -- @
  --
  -- @since 0.1
  (.%.) :: g -> g -> g

infixl 7 .%.

-- | @since 0.1
mnegate :: (MGroup g) => g -> g
mnegate n = one .%. n
{-# INLINE mnegate #-}

-- | @since 0.1
instance (Fractional a) => MGroup (AsFractional a) where
  (.%.) = coerce @(a -> a -> a) @(AsFractional a -> AsFractional a -> AsFractional a) (/)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance (Integral a) => MGroup (AsIntegral a) where
  (.%.) = coerce @(a -> a -> a) @(AsIntegral a -> AsIntegral a -> AsIntegral a) div
  {-# INLINE (.%.) #-}

-- | @since 0.1
deriving via (AsFractional Double) instance MGroup Double

-- | @since 0.1
deriving via (AsFractional Float) instance MGroup Float

-- | @since 0.1
deriving via (AsIntegral Int) instance MGroup Int

-- | @since 0.1
deriving via (AsIntegral Int8) instance MGroup Int8

-- | @since 0.1
deriving via (AsIntegral Int16) instance MGroup Int16

-- | @since 0.1
deriving via (AsIntegral Int32) instance MGroup Int32

-- | @since 0.1
deriving via (AsIntegral Int64) instance MGroup Int64

-- | @since 0.1
deriving via (AsIntegral Integer) instance MGroup Integer

-- | @since 0.1
deriving via (AsIntegral Word) instance MGroup Word

-- | @since 0.1
deriving via (AsIntegral Word8) instance MGroup Word8

-- | @since 0.1
deriving via (AsIntegral Word16) instance MGroup Word16

-- | @since 0.1
deriving via (AsIntegral Word32) instance MGroup Word32

-- | @since 0.1
deriving via (AsIntegral Word64) instance MGroup Word64

-- | @since 0.1
deriving via (AsIntegral Natural) instance MGroup Natural

-- | @since 0.1
deriving via (AsFractional (Ratio Integer)) instance MGroup (Ratio Integer)

-- | @since 0.1
deriving via (AsFractional (Ratio Natural)) instance MGroup (Ratio Natural)

-- | @since 0.1
deriving via (AsFractional (Complex a)) instance (RealFloat a) => MGroup (Complex a)

-- | @since 0.1
deriving via (AsFractional (Fixed k)) instance (HasResolution k) => MGroup (Fixed k)

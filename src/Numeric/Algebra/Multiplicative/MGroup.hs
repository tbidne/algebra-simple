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
  ( FromFractional (MkFromFractional),
    FromIntegral (MkFromIntegral),
  )
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid (one))

-- | Defines a multiplicative group.
--
-- @since 0.1
type MGroup :: Type -> Constraint
class (MMonoid g) => MGroup g where
  -- | @since 0.1
  (.%.) :: g -> g -> g

infixl 7 .%.

-- | @since 0.1
mnegate :: (MGroup g) => g -> g
mnegate n = one .%. n
{-# INLINE mnegate #-}

-- | @since 0.1
instance (Fractional a) => MGroup (FromFractional a) where
  (.%.) = coerce @(a -> a -> a) @(FromFractional a -> FromFractional a -> FromFractional a) (/)
  {-# INLINE (.%.) #-}

-- | @since 0.1
instance (Integral a) => MGroup (FromIntegral a) where
  (.%.) = coerce @(a -> a -> a) @(FromIntegral a -> FromIntegral a -> FromIntegral a) div
  {-# INLINE (.%.) #-}

-- | @since 0.1
deriving via (FromFractional Double) instance MGroup Double

-- | @since 0.1
deriving via (FromFractional Float) instance MGroup Float

-- | @since 0.1
deriving via (FromIntegral Int) instance MGroup Int

-- | @since 0.1
deriving via (FromIntegral Int8) instance MGroup Int8

-- | @since 0.1
deriving via (FromIntegral Int16) instance MGroup Int16

-- | @since 0.1
deriving via (FromIntegral Int32) instance MGroup Int32

-- | @since 0.1
deriving via (FromIntegral Int64) instance MGroup Int64

-- | @since 0.1
deriving via (FromIntegral Integer) instance MGroup Integer

-- | @since 0.1
deriving via (FromIntegral Word) instance MGroup Word

-- | @since 0.1
deriving via (FromIntegral Word8) instance MGroup Word8

-- | @since 0.1
deriving via (FromIntegral Word16) instance MGroup Word16

-- | @since 0.1
deriving via (FromIntegral Word32) instance MGroup Word32

-- | @since 0.1
deriving via (FromIntegral Word64) instance MGroup Word64

-- | @since 0.1
deriving via (FromIntegral Natural) instance MGroup Natural

-- | @since 0.1
deriving via (FromFractional (Ratio Integer)) instance MGroup (Ratio Integer)

-- | @since 0.1
deriving via (FromFractional (Ratio Natural)) instance MGroup (Ratio Natural)

-- | @since 0.1
deriving via (FromFractional (Complex a)) instance (RealFloat a) => MGroup (Complex a)

-- | @since 0.1
deriving via (FromFractional (Fixed k)) instance (HasResolution k) => MGroup (Fixed k)

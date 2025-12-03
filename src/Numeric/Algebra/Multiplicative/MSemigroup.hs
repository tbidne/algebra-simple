-- | Provides the 'MSemigroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MSemigroup
  ( MSemigroup (..),
  )
where

import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Deriving
  ( FromFractional (MkFromFractional),
    FromIntegral (MkFromIntegral),
    FromNum (MkFromNum),
  )

-- | Defines a multiplicative semigroup.
--
-- @since 0.1
type MSemigroup :: Type -> Constraint
class MSemigroup s where
  -- | @since 0.1
  (.*.) :: s -> s -> s

infixl 7 .*.

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => MSemigroup (FromFractional a)

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => MSemigroup (FromIntegral a)

-- | @since 0.1
instance (Num a) => MSemigroup (FromNum a) where
  (.*.) = coerce @(a -> a -> a) @(FromNum a -> FromNum a -> FromNum a) (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
deriving via (FromNum Double) instance MSemigroup Double

-- | @since 0.1
deriving via (FromNum Float) instance MSemigroup Float

-- | @since 0.1
deriving via (FromNum Int) instance MSemigroup Int

-- | @since 0.1
deriving via (FromNum Int8) instance MSemigroup Int8

-- | @since 0.1
deriving via (FromNum Int16) instance MSemigroup Int16

-- | @since 0.1
deriving via (FromNum Int32) instance MSemigroup Int32

-- | @since 0.1
deriving via (FromNum Int64) instance MSemigroup Int64

-- | @since 0.1
deriving via (FromNum Integer) instance MSemigroup Integer

-- | @since 0.1
deriving via (FromNum Word) instance MSemigroup Word

-- | @since 0.1
deriving via (FromNum Word8) instance MSemigroup Word8

-- | @since 0.1
deriving via (FromNum Word16) instance MSemigroup Word16

-- | @since 0.1
deriving via (FromNum Word32) instance MSemigroup Word32

-- | @since 0.1
deriving via (FromNum Word64) instance MSemigroup Word64

-- | @since 0.1
deriving via (FromNum Natural) instance MSemigroup Natural

-- | @since 0.1
deriving via (FromNum (Ratio Integer)) instance MSemigroup (Ratio Integer)

-- | @since 0.1
deriving via (FromNum (Ratio Natural)) instance MSemigroup (Ratio Natural)

-- | @since 0.1
deriving via (FromNum (Complex a)) instance (RealFloat a) => MSemigroup (Complex a)

-- | @since 0.1
deriving via (FromNum (Fixed k)) instance (HasResolution k) => MSemigroup (Fixed k)

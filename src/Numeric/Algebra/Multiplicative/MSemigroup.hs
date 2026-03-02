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
  ( AsFractional (MkAsFractional),
    AsIntegral (MkAsIntegral),
    AsNum (MkAsNum),
  )

-- | Defines a multiplicative semigroup.
--
-- ==== __Examples:__
--
-- >>> :{
--   -- Multiplication
--   f1 :: (MSemigroup g) => g -> g
--   f1 x = x .*. x
-- :}
--
-- >>> f1 5
-- 25
--
-- @since 0.1
type MSemigroup :: Type -> Constraint
class MSemigroup s where
  -- | Should satisfy:
  --
  -- @
  -- -- associativity
  -- a .*. (b .*. c) === (a .*. b) .*. c
  -- @
  --
  -- @since 0.1
  (.*.) :: s -> s -> s

infixl 7 .*.

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => MSemigroup (AsFractional a)

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => MSemigroup (AsIntegral a)

-- | @since 0.1
instance (Num a) => MSemigroup (AsNum a) where
  (.*.) = coerce @(a -> a -> a) @(AsNum a -> AsNum a -> AsNum a) (*)
  {-# INLINE (.*.) #-}

-- | @since 0.1
deriving via (AsNum Double) instance MSemigroup Double

-- | @since 0.1
deriving via (AsNum Float) instance MSemigroup Float

-- | @since 0.1
deriving via (AsNum Int) instance MSemigroup Int

-- | @since 0.1
deriving via (AsNum Int8) instance MSemigroup Int8

-- | @since 0.1
deriving via (AsNum Int16) instance MSemigroup Int16

-- | @since 0.1
deriving via (AsNum Int32) instance MSemigroup Int32

-- | @since 0.1
deriving via (AsNum Int64) instance MSemigroup Int64

-- | @since 0.1
deriving via (AsNum Integer) instance MSemigroup Integer

-- | @since 0.1
deriving via (AsNum Word) instance MSemigroup Word

-- | @since 0.1
deriving via (AsNum Word8) instance MSemigroup Word8

-- | @since 0.1
deriving via (AsNum Word16) instance MSemigroup Word16

-- | @since 0.1
deriving via (AsNum Word32) instance MSemigroup Word32

-- | @since 0.1
deriving via (AsNum Word64) instance MSemigroup Word64

-- | @since 0.1
deriving via (AsNum Natural) instance MSemigroup Natural

-- | @since 0.1
deriving via (AsNum (Ratio Integer)) instance MSemigroup (Ratio Integer)

-- | @since 0.1
deriving via (AsNum (Ratio Natural)) instance MSemigroup (Ratio Natural)

-- | @since 0.1
deriving via (AsNum (Complex a)) instance (RealFloat a) => MSemigroup (Complex a)

-- | @since 0.1
deriving via (AsNum (Fixed k)) instance (HasResolution k) => MSemigroup (Fixed k)

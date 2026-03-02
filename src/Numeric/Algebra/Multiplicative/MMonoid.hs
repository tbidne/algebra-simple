{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'MMonoid' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Multiplicative.MMonoid
  ( MMonoid (..),
    pattern One,
    pattern NonOne,
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
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup)

-- $setup
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a monoid over a multiplicative semigroup.
--
-- ==== __Examples:__
--
-- >>> :{
--   -- Multiplication
--   f1 :: (MMonoid g) => g -> g
--   f1 x = x .*. x
-- :}
--
-- >>> f1 5
-- 25
--
-- >>> :{
--   -- One
--   f2 :: (MMonoid g) => g -> g
--   f2 x = x .*. one
-- :}
--
-- >>> f2 5
-- 5
--
-- @since 0.1
type MMonoid :: Type -> Constraint
class (MSemigroup m) => MMonoid m where
  -- | @since 0.1
  one :: m

-- | Pattern synonym for 'one'.
--
-- @since 0.1
pattern One :: (MMonoid m, Eq m) => m
pattern One <- ((== one) -> True)
  where
    One = one

-- | Pattern synonym for @x /= 'one'@.
--
-- @since 0.1
pattern NonOne :: (MMonoid m, Eq m) => m -> m
pattern NonOne y <- (\x -> (x == one, x) -> (False, y))

-- see NOTE: [Pattern Synonym COMPLETE]

#if MIN_VERSION_base(4, 16, 0)
{-# COMPLETE One, NonOne #-}
#endif

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => MMonoid (AsFractional a)

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => MMonoid (AsIntegral a)

-- | @since 0.1
instance (Num a) => MMonoid (AsNum a) where
  one = coerce @a 1
  {-# INLINE one #-}

-- | @since 0.1
deriving via (AsNum Double) instance MMonoid Double

-- | @since 0.1
deriving via (AsNum Float) instance MMonoid Float

-- | @since 0.1
deriving via (AsNum Int) instance MMonoid Int

-- | @since 0.1
deriving via (AsNum Int8) instance MMonoid Int8

-- | @since 0.1
deriving via (AsNum Int16) instance MMonoid Int16

-- | @since 0.1
deriving via (AsNum Int32) instance MMonoid Int32

-- | @since 0.1
deriving via (AsNum Int64) instance MMonoid Int64

-- | @since 0.1
deriving via (AsNum Integer) instance MMonoid Integer

-- | @since 0.1
deriving via (AsNum Word) instance MMonoid Word

-- | @since 0.1
deriving via (AsNum Word8) instance MMonoid Word8

-- | @since 0.1
deriving via (AsNum Word16) instance MMonoid Word16

-- | @since 0.1
deriving via (AsNum Word32) instance MMonoid Word32

-- | @since 0.1
deriving via (AsNum Word64) instance MMonoid Word64

-- | @since 0.1
deriving via (AsNum Natural) instance MMonoid Natural

-- | @since 0.1
deriving via (AsNum (Ratio Integer)) instance MMonoid (Ratio Integer)

-- | @since 0.1
deriving via (AsNum (Ratio Natural)) instance MMonoid (Ratio Natural)

-- | @since 0.1
deriving via (AsNum (Complex a)) instance (RealFloat a) => MMonoid (Complex a)

-- | @since 0.1
deriving via (AsNum (Fixed k)) instance (HasResolution k) => MMonoid (Fixed k)

-- | Provides the 'Semiring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Semiring
  ( Semiring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Rings.Demiring (Demiring)
import Numeric.Algebra.Rings.Hemiring (Hemiring)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a semiring i.e. a structure that is both an 'Hemiring' and
-- 'Demiring'. In other words, a structure that is both an additive and
-- multiplicative monoid.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{+} \), the non-negative integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Semiring r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (Semiring r) => r -> r
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Multiplication
--   f3 :: (Semiring r) => r -> r
--   f3 x = x .*. x
-- :}
--
-- >>> f3 5
-- 25
--
-- >>> :{
--   -- One
--   f4 :: (Semiring r) => r -> r
--   f4 x = x .*. one
-- :}
--
-- >>> f4 5
-- 5
--
-- @since 0.1
type Semiring :: Type -> Constraint
class (Demiring r, Hemiring r) => Semiring r

-- | @since 0.1
instance Semiring Double

-- | @since 0.1
instance Semiring Float

-- | @since 0.1
instance Semiring Int

-- | @since 0.1
instance Semiring Int8

-- | @since 0.1
instance Semiring Int16

-- | @since 0.1
instance Semiring Int32

-- | @since 0.1
instance Semiring Int64

-- | @since 0.1
instance Semiring Integer

-- | @since 0.1
instance Semiring Natural

-- | @since 0.1
instance Semiring Word

-- | @since 0.1
instance Semiring Word8

-- | @since 0.1
instance Semiring Word16

-- | @since 0.1
instance Semiring Word32

-- | @since 0.1
instance Semiring Word64

-- | @since 0.1
instance Semiring (Ratio Integer)

-- | @since 0.1
instance Semiring (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Semiring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Semiring (Fixed k)

-- | Provides the 'Hemiring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Hemiring
  ( Hemiring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Rings.Quartaring (Quartaring)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a 'Hemiring' i.e. a 'Quartaring' with additive identity. In other
-- words, a semiring without multiplicative identity.
--
-- ==== __Examples:__
--
-- - \( \mathbb{N} \setminus \{1\} \), the naturals without 1.
--
-- >>> :{
--   -- Addition
--   f1 :: (Hemiring r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (Hemiring r) => r -> r
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Multiplication
--   f3 :: (Hemiring r) => r -> r
--   f3 x = x .*. x
-- :}
--
-- >>> f3 5
-- 25
--
-- @since 0.1
type Hemiring :: Type -> Constraint
class (AMonoid r, Quartaring r) => Hemiring r

-- | @since 0.1
instance Hemiring Double

-- | @since 0.1
instance Hemiring Float

-- | @since 0.1
instance Hemiring Int

-- | @since 0.1
instance Hemiring Int8

-- | @since 0.1
instance Hemiring Int16

-- | @since 0.1
instance Hemiring Int32

-- | @since 0.1
instance Hemiring Int64

-- | @since 0.1
instance Hemiring Integer

-- | @since 0.1
instance Hemiring Natural

-- | @since 0.1
instance Hemiring Word

-- | @since 0.1
instance Hemiring Word8

-- | @since 0.1
instance Hemiring Word16

-- | @since 0.1
instance Hemiring Word32

-- | @since 0.1
instance Hemiring Word64

-- | @since 0.1
instance Hemiring (Ratio Integer)

-- | @since 0.1
instance Hemiring (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Hemiring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Hemiring (Fixed k)

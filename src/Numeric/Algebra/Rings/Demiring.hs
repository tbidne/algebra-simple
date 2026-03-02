-- | Provides the 'Demiring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Demiring
  ( Demiring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Multiplicative.MMonoid (MMonoid)
import Numeric.Algebra.Rings.Quartaring (Quartaring)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a 'Demiring' i.e. a 'Quartaring' with multiplicative identity.
-- In other words, a semiring without additive identity.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{\times} \), the positive integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Demiring r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Multiplication
--   f2 :: (Demiring r) => r -> r
--   f2 x = x .*. x
-- :}
--
-- >>> f2 5
-- 25
--
-- >>> :{
--   -- One
--   f3 :: (Demiring r) => r -> r
--   f3 x = x .*. one
-- :}
--
-- >>> f3 5
-- 5
--
-- @since 0.1
type Demiring :: Type -> Constraint
class (MMonoid r, Quartaring r) => Demiring r

-- | @since 0.1
instance Demiring Double

-- | @since 0.1
instance Demiring Float

-- | @since 0.1
instance Demiring Int

-- | @since 0.1
instance Demiring Int8

-- | @since 0.1
instance Demiring Int16

-- | @since 0.1
instance Demiring Int32

-- | @since 0.1
instance Demiring Int64

-- | @since 0.1
instance Demiring Integer

-- | @since 0.1
instance Demiring Natural

-- | @since 0.1
instance Demiring Word

-- | @since 0.1
instance Demiring Word8

-- | @since 0.1
instance Demiring Word16

-- | @since 0.1
instance Demiring Word32

-- | @since 0.1
instance Demiring Word64

-- | @since 0.1
instance Demiring (Ratio Integer)

-- | @since 0.1
instance Demiring (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Demiring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Demiring (Fixed k)

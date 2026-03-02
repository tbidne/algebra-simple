-- | Provides the 'PseudoRing' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.PseudoRing
  ( PseudoRing,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AGroup (AGroup)
import Numeric.Algebra.Rings.Hemiring (Hemiring)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a 'PseudoRing' i.e. a ring without multiplicative identity.
-- Often called an Rng.
--
-- ==== __Examples:__
--
-- - \( 2 \mathbb{Z} \), the even integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (PseudoRing r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (PseudoRing r) => r -> r
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Subtraction
--   f3 :: (PseudoRing r, Num r) => r -> r
--   f3 x = x .-. 3
-- :}
--
-- >>> f3 5
-- 2
--
-- >>> :{
--   -- Multiplication
--   f4 :: (PseudoRing r) => r -> r
--   f4 x = x .*. x
-- :}
--
-- >>> f4 5
-- 25
--
-- @since 0.1
type PseudoRing :: Type -> Constraint
class (AGroup r, Hemiring r) => PseudoRing r

-- | @since 0.1
instance PseudoRing Double

-- | @since 0.1
instance PseudoRing Float

-- | @since 0.1
instance PseudoRing Int

-- | @since 0.1
instance PseudoRing Int8

-- | @since 0.1
instance PseudoRing Int16

-- | @since 0.1
instance PseudoRing Int32

-- | @since 0.1
instance PseudoRing Int64

-- | @since 0.1
instance PseudoRing Integer

-- | @since 0.1
instance PseudoRing Word

-- | @since 0.1
instance PseudoRing Word8

-- | @since 0.1
instance PseudoRing Word16

-- | @since 0.1
instance PseudoRing Word32

-- | @since 0.1
instance PseudoRing Word64

-- | @since 0.1
instance PseudoRing (Ratio Integer)

-- | @since 0.1
instance (RealFloat a) => PseudoRing (Complex a)

-- | @since 0.1
instance (HasResolution k) => PseudoRing (Fixed k)

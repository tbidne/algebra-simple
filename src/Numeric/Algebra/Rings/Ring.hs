-- | Provides the 'Ring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Ring
  ( Ring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Rings.PseudoRing (PseudoRing)
import Numeric.Algebra.Rings.Semiring (Semiring)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)

-- | Defines a 'Ring' i.e. a structure that is an 'PseudoRing' and a 'Semiring'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z} \), the integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Ring r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (Ring r) => r -> r
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Subtraction
--   f3 :: (Ring r, Num r) => r -> r
--   f3 x = x .-. 3
-- :}
--
-- >>> f3 5
-- 2
--
-- >>> :{
--   -- Multiplication
--   f4 :: (Ring r) => r -> r
--   f4 x = x .*. x
-- :}
--
-- >>> f4 5
-- 25
--
-- >>> :{
--   -- One
--   f5 :: (Ring r) => r -> r
--   f5 x = x .*. one
-- :}
--
-- >>> f5 5
-- 5
--
-- @since 0.1
type Ring :: Type -> Constraint
class (PseudoRing r, Semiring r) => Ring r

-- | @since 0.1
instance Ring Double

-- | @since 0.1
instance Ring Float

-- | @since 0.1
instance Ring Int

-- | @since 0.1
instance Ring Int8

-- | @since 0.1
instance Ring Int16

-- | @since 0.1
instance Ring Int32

-- | @since 0.1
instance Ring Int64

-- | @since 0.1
instance Ring Integer

-- | @since 0.1
instance Ring Word

-- | @since 0.1
instance Ring Word8

-- | @since 0.1
instance Ring Word16

-- | @since 0.1
instance Ring Word32

-- | @since 0.1
instance Ring Word64

-- | @since 0.1
instance Ring (Ratio Integer)

-- | @since 0.1
instance (RealFloat a) => Ring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Ring (Fixed k)

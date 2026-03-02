-- | Provides the 'Semifield' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Semifield
  ( Semifield,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Rings.PseudoSemifield (PseudoSemifield)
import Numeric.Algebra.Rings.Semiring (Semiring)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))

-- | Defines a 'Semifield' i.e. a structure that is an 'PseudoSemifield' and a 'Semiring'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q}^{+} \), the non-negative rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (Semifield k) => k -> k
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (Semifield k) => k -> k
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Multiplication
--   f3 :: (Semifield k) => k -> k
--   f3 x = x .*. x
-- :}
--
-- >>> f3 5
-- 25
--
-- >>> :{
--   -- One
--   f4 :: (Semifield k) => k -> k
--   f4 x = x .*. one
-- :}
--
-- >>> f4 5
-- 5
--
-- >>> :{
--   -- Division
--   f5 :: (Semifield k, Num k) => k -> k
--   f5 x = x .%. 2
-- :}
--
-- >>> f5 6
-- 3
--
-- @since 0.1
type Semifield :: Type -> Constraint
class (PseudoSemifield r, Semiring r) => Semifield r

-- | @since 0.1
instance Semifield Double

-- | @since 0.1
instance Semifield Float

-- | @since 0.1
instance Semifield Int

-- | @since 0.1
instance Semifield Int8

-- | @since 0.1
instance Semifield Int16

-- | @since 0.1
instance Semifield Int32

-- | @since 0.1
instance Semifield Int64

-- | @since 0.1
instance Semifield Integer

-- | @since 0.1
instance Semifield Natural

-- | @since 0.1
instance Semifield Word

-- | @since 0.1
instance Semifield Word8

-- | @since 0.1
instance Semifield Word16

-- | @since 0.1
instance Semifield Word32

-- | @since 0.1
instance Semifield Word64

-- | @since 0.1
instance Semifield (Ratio Integer)

-- | @since 0.1
instance Semifield (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Semifield (Complex a)

-- | @since 0.1
instance (HasResolution k) => Semifield (Fixed k)

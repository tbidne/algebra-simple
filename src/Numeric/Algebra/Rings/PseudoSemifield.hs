-- | Provides the 'PseudoSemifield' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.PseudoSemifield
  ( PseudoSemifield,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Multiplicative.MGroup (MGroup)
import Numeric.Algebra.Rings.Demiring (Demiring)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))

-- | Defines a 'PseudoSemifield' i.e. a semifield without additive identity.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q}^{\times} \), the positive rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (PseudoSemifield k) => k -> k
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Multiplication
--   f2 :: (PseudoSemifield k) => k -> k
--   f2 x = x .*. x
-- :}
--
-- >>> f2 5
-- 25
--
-- >>> :{
--   -- One
--   f3 :: (PseudoSemifield k) => k -> k
--   f3 x = x .*. one
-- :}
--
-- >>> f3 5
-- 5
--
-- >>> :{
--   -- Division
--   f4 :: (PseudoSemifield k, Num k) => k -> k
--   f4 x = x .%. 2
-- :}
--
-- >>> f4 6
-- 3
--
-- @since 0.1
type PseudoSemifield :: Type -> Constraint
class (Demiring r, MGroup r) => PseudoSemifield r

-- | @since 0.1
instance PseudoSemifield Double

-- | @since 0.1
instance PseudoSemifield Float

-- | @since 0.1
instance PseudoSemifield Int

-- | @since 0.1
instance PseudoSemifield Int8

-- | @since 0.1
instance PseudoSemifield Int16

-- | @since 0.1
instance PseudoSemifield Int32

-- | @since 0.1
instance PseudoSemifield Int64

-- | @since 0.1
instance PseudoSemifield Integer

-- | @since 0.1
instance PseudoSemifield Natural

-- | @since 0.1
instance PseudoSemifield Word

-- | @since 0.1
instance PseudoSemifield Word8

-- | @since 0.1
instance PseudoSemifield Word16

-- | @since 0.1
instance PseudoSemifield Word32

-- | @since 0.1
instance PseudoSemifield Word64

-- | @since 0.1
instance PseudoSemifield (Ratio Integer)

-- | @since 0.1
instance PseudoSemifield (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => PseudoSemifield (Complex a)

-- | @since 0.1
instance (HasResolution k) => PseudoSemifield (Fixed k)

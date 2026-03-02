-- | Provides the 'Field' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Field
  ( Field,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Rings.Ring (Ring)
import Numeric.Algebra.Rings.Semifield (Semifield)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))

-- | Defines a 'Field' i.e. a structure that supports addition, subtraction,
-- multiplication, and division.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q} \), the rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (Field k) => k -> k
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (Field k) => k -> k
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- >>> :{
--   -- Subtraction
--   f3 :: (Field k, Num k) => k -> k
--   f3 x = x .-. 3
-- :}
--
-- >>> f3 5
-- 2
--
-- >>> :{
--   -- Multiplication
--   f4 :: (Field k) => k -> k
--   f4 x = x .*. x
-- :}
--
-- >>> f4 5
-- 25
--
-- >>> :{
--   -- One
--   f5 :: (Field k) => k -> k
--   f5 x = x .*. one
-- :}
--
-- >>> f5 5
-- 5
--
-- >>> :{
--   -- Division
--   f6 :: (Field k, Num k) => k -> k
--   f6 x = x .%. 2
-- :}
--
-- f6 6
-- 2
--
-- @since 0.1
type Field :: Type -> Constraint
class (Ring f, Semifield f) => Field f

-- | @since 0.1
instance Field Float

-- | @since 0.1
instance Field Double

-- | @since 0.1
instance Field Int

-- | @since 0.1
instance Field Int8

-- | @since 0.1
instance Field Int16

-- | @since 0.1
instance Field Int32

-- | @since 0.1
instance Field Int64

-- | @since 0.1
instance Field Integer

-- | @since 0.1
instance Field Word

-- | @since 0.1
instance Field Word8

-- | @since 0.1
instance Field Word16

-- | @since 0.1
instance Field Word32

-- | @since 0.1
instance Field Word64

-- | @since 0.1
instance Field (Ratio Integer)

-- | @since 0.1
instance (RealFloat a) => Field (Complex a)

-- | @since 0.1
instance (HasResolution k) => Field (Fixed k)

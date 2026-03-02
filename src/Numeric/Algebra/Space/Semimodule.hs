-- | Provides the 'Semimodule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Semimodule
  ( Semimodule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.Semiring (Semiring)
import Numeric.Algebra.Space.Demimodule (Demimodule)
import Numeric.Algebra.Space.Hemimodule (Hemimodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))

-- | Defines a 'Semimodule' over a 'Semiring'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{+} \times \mathbb{Z}^{+} \), the two-dimensional
--   non-negative integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Semimodule m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (Semimodule m r) => m -> m
--   f2 m = m .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Scalar multiplication
--   f3 :: (Semimodule m r, Num r) => m -> m
--   f3 m = m .* 6
-- :}
--
-- >>> f3 (8,4)
-- (48,24)
--
-- @since 0.1
type Semimodule :: Type -> Type -> Constraint
class (Demimodule m r, Hemimodule m r, Semiring r) => Semimodule m r | m -> r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Semiring r) => Semimodule (r, r, r, r, r, r, r, r, r) r

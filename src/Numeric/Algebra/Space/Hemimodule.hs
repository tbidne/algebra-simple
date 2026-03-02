-- | Provides the 'Hemimodule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Hemimodule
  ( Hemimodule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Additive.AMonoid (AMonoid)
import Numeric.Algebra.Rings.Hemiring (Hemiring)
import Numeric.Algebra.Space.Quartamodule (Quartamodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))

-- | Defines a 'Hemimodule' over a 'Hemiring'.
--
-- ==== __Examples:__
--
-- - \( \text{Let } S = \mathbb{N} \setminus \{1\} \text{ in } S \times S \),
--   two-dimensional naturals without 1.
--
-- >>> :{
--   -- Addition
--   f1 :: (Hemimodule m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (Hemimodule m r) => m -> m
--   f2 m = m .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Scalar multiplication
--   f3 :: (Hemimodule m r, Num r) => m -> m
--   f3 m = m .* 6
-- :}
--
-- >>> f3 (8,4)
-- (48,24)
--
-- @since 0.1
type Hemimodule :: Type -> Type -> Constraint
class (AMonoid m, Quartamodule m r, Hemiring r) => Hemimodule m r | m -> r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Hemiring r) => Hemimodule (r, r, r, r, r, r, r, r, r) r

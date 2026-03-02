-- | Provides the 'Demimodule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Demimodule
  ( Demimodule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.Demiring (Demiring)
import Numeric.Algebra.Space.Quartamodule (Quartamodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))

-- | Defines a 'Demimodule' over a 'Demiring'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{\times} \times \mathbb{Z}^{\times} \), the two-dimensional
--   positive integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Demimodule m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Scalar multiplication
--   f2 :: (Demimodule m r, Num r) => m -> m
--   f2 m = m .* 6
-- :}
--
-- >>> f2 (8,4)
-- (48,24)
--
-- @since 0.1
type Demimodule :: Type -> Type -> Constraint
class (Quartamodule m r, Demiring r) => Demimodule m r | m -> r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Demiring r) => Demimodule (r, r, r, r, r, r, r, r, r) r

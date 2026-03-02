-- | Provides the 'Quartamodule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Quartamodule
  ( Quartamodule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup)
import Numeric.Algebra.Rings.Quartaring (Quartaring)
import Numeric.Algebra.Space.MSemiSpace (MSemiSpace)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))

-- | Defines a 'Quartamodule' over a 'Quartaring'. For elements @x@, @y@ of a
-- 'Quartamodule' @M@ and @r@, @s@ in a 'Quartaring' @R@, we have the following
-- laws:
--
-- @
-- (x .+. y) .* r === (x .* r) .+. (y .* r)
-- x .* (r .+. s) === (x .* r) .+. (x .* s)
-- x .* (r .*. s) === (x .* r) .* s
-- x .* one === x -- when R is a Demiring (has multiplicative identity)
-- @
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{\gt 1} \times \mathbb{Z}^{\gt 1} \), two-dimensional
--   integers greater than one.
--
-- >>> :{
--   -- Addition
--   f1 :: (Quartamodule m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Scalar multiplication
--   f2 :: (Quartamodule m r, Num r) => m -> m
--   f2 m = m .* 6
-- :}
--
-- >>> f2 (8,4)
-- (48,24)
--
-- @since 0.1
type Quartamodule :: Type -> Type -> Constraint
class (ASemigroup m, MSemiSpace m r, Quartaring r) => Quartamodule m r | m -> r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Quartaring r) => Quartamodule (r, r, r, r, r, r, r, r, r) r

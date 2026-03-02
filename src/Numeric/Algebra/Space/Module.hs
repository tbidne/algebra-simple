-- | Provides the 'Module' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.Module
  ( Module,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.Ring (Ring)
import Numeric.Algebra.Space.PseudoModule (PseudoModule)
import Numeric.Algebra.Space.Semimodule (Semimodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))

-- | Defines a 'Module' over a 'Ring'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z} \times \mathbb{Z} \), the two-dimensional integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (Module m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (Module m r) => m -> m
--   f2 m = m .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Subtraction
--   f3 :: (Module m r) => m -> m
--   f3 m = m .-. m
-- :}
--
-- >>> f3 (8,4)
-- (0,0)
--
-- >>> :{
--   -- Scalar multiplication
--   f4 :: (Module m r, Num r) => m -> m
--   f4 m = m .* 6
-- :}
--
-- >>> f4 (8,4)
-- (48,24)
--
-- @since 0.1
type Module :: Type -> Type -> Constraint
class (PseudoModule m r, Semimodule m r, Ring r) => Module m r | m -> r

-- | @since 0.1
instance (Ring r) => Module (r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (Ring r) => Module (r, r, r, r, r, r, r, r, r) r

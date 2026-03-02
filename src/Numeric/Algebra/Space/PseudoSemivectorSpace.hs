-- | Provides the 'PseudoSemivectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.PseudoSemivectorSpace
  ( PseudoSemivectorSpace,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.PseudoSemifield (PseudoSemifield)
import Numeric.Algebra.Space.Demimodule (Demimodule)
import Numeric.Algebra.Space.MSpace (MSpace)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))
-- >>> import Numeric.Algebra.Space.MSpace ((.%))

-- | Defines a 'PseudoSemivectorSpace' over a 'PseudoSemifield'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q}^{\times} \times \mathbb{Q}^{\times} \), the two-dimensional
--   positive rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (PseudoSemivectorSpace v k) => v -> v
--   f1 v = v .+. v
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Scalar multiplication
--   f2 :: (PseudoSemivectorSpace v k, Num k) => v -> v
--   f2 v = v .* 6
-- :}
--
-- >>> f2 (8,4)
-- (48,24)
--
-- >>> :{
--   -- Scalar division
--   f3 :: (PseudoSemivectorSpace v k, Num k) => v -> v
--   f3 v = v .% 2
-- :}
--
-- >>> f3 (8,4)
-- (4,2)
--
-- @since 0.1
type PseudoSemivectorSpace :: Type -> Type -> Constraint
class (MSpace m r, Demimodule m r, PseudoSemifield r) => PseudoSemivectorSpace m r | m -> r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoSemifield r) => PseudoSemivectorSpace (r, r, r, r, r, r, r, r, r) r

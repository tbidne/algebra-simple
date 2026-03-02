-- | Provides the 'SemivectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.SemivectorSpace
  ( SemivectorSpace,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.Semifield (Semifield)
import Numeric.Algebra.Space.PseudoSemivectorSpace (PseudoSemivectorSpace)
import Numeric.Algebra.Space.Semimodule (Semimodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))
-- >>> import Numeric.Algebra.Space.MSpace ((.%))

-- | Defines a 'SemivectorSpace' over a 'Semifield'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q}^{+} \times \mathbb{Q}^{+} \), the two-dimensional
--   non-negative rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (SemivectorSpace v k) => v -> v
--   f1 v = v .+. v
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (SemivectorSpace v k) => v -> v
--   f2 v = v .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Scalar multiplication
--   f3 :: (SemivectorSpace v k, Num k) => v -> v
--   f3 v = v .* 6
-- :}
--
-- >>> f3 (8,4)
-- (48,24)
--
-- >>> :{
--   -- Scalar division
--   f4 :: (SemivectorSpace v k, Num k) => v -> v
--   f4 v = v .% 2
-- :}
--
-- >>> f4 (8,4)
-- (4,2)
--
-- @since 0.1
type SemivectorSpace :: Type -> Type -> Constraint
class (PseudoSemivectorSpace v k, Semimodule v k, Semifield k) => SemivectorSpace v k | v -> k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k, k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k, k, k, k, k) k

-- | @since 0.1
instance (Semifield k) => SemivectorSpace (k, k, k, k, k, k, k, k, k) k

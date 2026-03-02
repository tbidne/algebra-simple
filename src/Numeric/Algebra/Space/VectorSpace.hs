-- | Provides the 'VectorSpace' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.VectorSpace
  ( VectorSpace,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Rings.Field (Field)
import Numeric.Algebra.Space.Module (Module)
import Numeric.Algebra.Space.SemivectorSpace (SemivectorSpace)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))
-- >>> import Numeric.Algebra.Space.MSpace ((.%))

-- | Defines a 'VectorSpace' over a 'Field'.
--
-- ==== __Examples:__
--
-- - \( \mathbb{Q} \times \mathbb{Q} \), the two-dimensional rationals.
--
-- >>> :{
--   -- Addition
--   f1 :: (VectorSpace v k) => v -> v
--   f1 v = v .+. v
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (VectorSpace v k) => v -> v
--   f2 v = v .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Subtraction
--   f3 :: (VectorSpace v k) => v -> v
--   f3 v = v .-. v
-- :}
--
-- >>> f3 (8,4)
-- (0,0)
--
-- >>> :{
--   -- Scalar multiplication
--   f4 :: (VectorSpace v k, Num k) => v -> v
--   f4 v = v .* 6
-- :}
--
-- >>> f4 (8,4)
-- (48,24)
--
-- >>> :{
--   -- Scalar division
--   f5 :: (VectorSpace v k, Num k) => v -> v
--   f5 v = v .% 2
-- :}
--
-- >>> f5 (8,4)
-- (4,2)
--
-- @since 0.1
type VectorSpace :: Type -> Type -> Constraint
class (Field k, Module v k, SemivectorSpace v k) => VectorSpace v k | v -> k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k, k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k, k, k, k, k) k

-- | @since 0.1
instance (Field k) => VectorSpace (k, k, k, k, k, k, k, k, k) k

-- | Provides the 'PseudoModule' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Space.PseudoModule
  ( PseudoModule,
  )
where

import Data.Kind (Constraint, Type)
import Numeric.Algebra.Additive.AGroup (AGroup)
import Numeric.Algebra.Rings.PseudoRing (PseudoRing)
import Numeric.Algebra.Space.Hemimodule (Hemimodule)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Additive.AMonoid (zero)
-- >>> import Numeric.Algebra.Additive.AGroup ((.-.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))
-- >>> import Numeric.Algebra.Multiplicative.MMonoid (one)
-- >>> import Numeric.Algebra.Multiplicative.MGroup ((.%.))
-- >>> import Numeric.Algebra.Space.MSemiSpace ((.*))
-- >>> import Numeric.Algebra.Space.MSpace ((.%))

-- | Defines a 'PseudoModule' over an 'PseudoRing'.
--
-- ==== __Examples:__
--
-- - \( 2 \mathbb{Z} \times 2 \mathbb{Z} \), the two-dimensional
--   even integers.
--
-- >>> :{
--   -- Addition
--   f1 :: (PseudoModule m r) => m -> m
--   f1 m = m .+. m
-- :}
--
-- >>> f1 (8,4)
-- (16,8)
--
-- >>> :{
--   -- Zero
--   f2 :: (PseudoModule m r) => m -> m
--   f2 m = m .+. zero
-- :}
--
-- >>> f2 (8,4)
-- (8,4)
--
-- >>> :{
--   -- Subtraction
--   f3 :: (PseudoModule m r) => m -> m
--   f3 m = m .-. m
-- :}
--
-- >>> f3 (8,4)
-- (0,0)
--
-- >>> :{
--   -- Scalar multiplication
--   f4 :: (PseudoModule m r, Num r) => m -> m
--   f4 m = m .* 6
-- :}
--
-- >>> f4 (8,4)
-- (48,24)
--
-- @since 0.1
type PseudoModule :: Type -> Type -> Constraint
class (AGroup m, Hemimodule m r, PseudoRing r) => PseudoModule m r | m -> r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r, r, r, r, r) r

-- | @since 0.1
instance (PseudoRing r) => PseudoModule (r, r, r, r, r, r, r, r, r) r

-- | Provides the 'NonPositive' type for enforcing a non-positive invariant.
--
-- @since 0.1.0.0
module Simple.Algebra.Data.NonPositive
  ( -- * Type
    NonPositive (MkNonPositive, unNonPositive),

    -- * Creation
    mkNonPositive,
    readNonPositive,
    unsafeNonPositive,
  )
where

import Simple.Algebra.Additive (Additive (..))
import Simple.Algebra.AdditiveMonoid (AdditiveMonoid (..))
import Simple.Algebra.Data.Utils qualified as U

-- | Newtype wrapper over /a/. The underlying /a/ is in \(-(\infty, 0]\).
--
-- @since 0.1.0.0
newtype NonPositive a = UnsafeNonPositive
  { -- | Unwraps the 'NonPositive'
    --
    -- @since 0.1.0.0
    unNonPositive :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonPositive'.
--
-- @since 0.1.0.0
pattern MkNonPositive :: a -> NonPositive a
pattern MkNonPositive n <- UnsafeNonPositive n

{-# COMPLETE MkNonPositive #-}

-- | @since 0.1.0.0
instance (Num a, Ord a) => Additive (NonPositive a) where
  MkNonPositive x .+. MkNonPositive y = unsafeNonPositive $ x + y

-- | @since 0.1.0.0
instance (Num a, Ord a) => AdditiveMonoid (NonPositive a) where
  zero = unsafeNonPositive 0

-- | Smart constructor for 'NonPositive'.
--
-- Examples:
--
-- >>> mkNonPositive (-2)
-- Just (UnsafeNonPositive {unNonPositive = -2})
--
-- >>> mkNonPositive 7
-- Nothing
--
-- @since 0.1.0.0
mkNonPositive :: (Num a, Ord a) => a -> Maybe (NonPositive a)
mkNonPositive = U.mkX isNonPositive UnsafeNonPositive

-- | Unsafe constructor for 'NonPositive', intended to be used with
-- known constants, e.g., @unsafeNonPositive (-2)@. Exercise restraint!
--
-- >>> unsafeNonPositive 0
-- UnsafeNonPositive {unNonPositive = 0}
--
-- >>> unsafeNonPositive 7
-- Passed positive to unsafeNonPositive!
--
-- @since 0.1.0.0
unsafeNonPositive :: (Num a, Ord a) => a -> NonPositive a
unsafeNonPositive = U.unsafeX msg isNonPositive UnsafeNonPositive
  where
    msg = "Passed positive to unsafeNonPositive!"

-- | Safely attempts to read a 'NonPositive'.
--
-- >>> readNonPositive "-5"
-- Just (UnsafeNonPositive {unNonPositive = -5})
--
-- >>> readNonPositive "cat"
-- Nothing
--
-- >>> readNonPositive "5"
-- Nothing
--
-- @since 0.1.0.0
readNonPositive :: (Num a, Ord a, Read a) => String -> Maybe (NonPositive a)
readNonPositive = U.readX isNonPositive UnsafeNonPositive

isNonPositive :: (Num a, Ord a) => a -> Bool
isNonPositive = (<= 0)
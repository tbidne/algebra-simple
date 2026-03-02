{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AMonoid
  ( AMonoid (..),
    pattern Zero,
    pattern NonZero,
  )
where

import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup)
import Numeric.Algebra.Deriving
  ( AsFractional (MkAsFractional),
    AsIntegral (MkAsIntegral),
    AsNum (MkAsNum),
  )

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))

-- | Defines a monoid over an additive semigroup.
--
-- ==== __Examples:__
--
-- >>> :{
--   -- Addition
--   f1 :: (AMonoid g) => g -> g
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Zero
--   f2 :: (AMonoid g) => g -> g
--   f2 x = x .+. zero
-- :}
--
-- >>> f2 5
-- 5
--
-- @since 0.1
type AMonoid :: Type -> Constraint
class (ASemigroup m) => AMonoid m where
  -- | Should satisfy:
  --
  -- @
  -- -- identity
  -- x .+. zero = x = zero .+. x
  -- @since 0.1
  zero :: m

-- | Pattern synonym for 'zero'.
--
-- @since 0.1
pattern Zero :: (AMonoid m, Eq m) => m
pattern Zero <- ((== zero) -> True)
  where
    Zero = zero

-- | Pattern synonym for @x /= 'zero'@.
--
-- @since 0.1
pattern NonZero :: (AMonoid m, Eq m) => m -> m
pattern NonZero y <- (\x -> (x == zero, x) -> (False, y))

-- NOTE: [Pattern Synonym COMPLETE]
--
-- Such COMPLETE pragmas not supported in GHC < 9.2:
--
-- - https://gitlab.haskell.org/ghc/ghc/-/issues/19160
-- - https://gitlab.haskell.org/ghc/ghc/-/issues/14422

#if MIN_VERSION_base(4, 16, 0)
{-# COMPLETE Zero, NonZero #-}
#endif

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => AMonoid (AsFractional a)

-- | @since 0.1
deriving via (AsNum a) instance (Num a) => AMonoid (AsIntegral a)

-- | @since 0.1
instance (Num a) => AMonoid (AsNum a) where
  zero = coerce @a 0
  {-# INLINE zero #-}

-- | @since 0.1
deriving via (AsNum Double) instance AMonoid Double

-- | @since 0.1
deriving via (AsNum Float) instance AMonoid Float

-- | @since 0.1
deriving via (AsNum Int) instance AMonoid Int

-- | @since 0.1
deriving via (AsNum Int8) instance AMonoid Int8

-- | @since 0.1
deriving via (AsNum Int16) instance AMonoid Int16

-- | @since 0.1
deriving via (AsNum Int32) instance AMonoid Int32

-- | @since 0.1
deriving via (AsNum Int64) instance AMonoid Int64

-- | @since 0.1
deriving via (AsNum Integer) instance AMonoid Integer

-- | @since 0.1
deriving via (AsNum Word) instance AMonoid Word

-- | @since 0.1
deriving via (AsNum Word8) instance AMonoid Word8

-- | @since 0.1
deriving via (AsNum Word16) instance AMonoid Word16

-- | @since 0.1
deriving via (AsNum Word32) instance AMonoid Word32

-- | @since 0.1
deriving via (AsNum Word64) instance AMonoid Word64

-- | @since 0.1
deriving via (AsNum Natural) instance AMonoid Natural

-- | @since 0.1
deriving via (AsNum (Ratio Integer)) instance AMonoid (Ratio Integer)

-- | @since 0.1
deriving via (AsNum (Ratio Natural)) instance AMonoid (Ratio Natural)

-- | @since 0.1
deriving via (AsNum (Complex a)) instance (RealFloat a) => AMonoid (Complex a)

-- | @since 0.1
deriving via (AsNum (Fixed k)) instance (HasResolution k) => AMonoid (Fixed k)

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a) where
  zero = (zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a) where
  zero = (zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a) where
  zero = (zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

-- | @since 0.1
instance (AMonoid a) => AMonoid (a, a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)
  {-# INLINE zero #-}

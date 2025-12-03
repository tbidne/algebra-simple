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
  ( FromFractional (MkFromFractional),
    FromIntegral (MkFromIntegral),
    FromNum (MkFromNum),
  )

-- | Defines a monoid over an additive semigroup.
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
deriving via (FromNum a) instance (Num a) => AMonoid (FromFractional a)

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => AMonoid (FromIntegral a)

-- | @since 0.1
instance (Num a) => AMonoid (FromNum a) where
  zero = coerce @a 0
  {-# INLINE zero #-}

-- | @since 0.1
deriving via (FromNum Double) instance AMonoid Double

-- | @since 0.1
deriving via (FromNum Float) instance AMonoid Float

-- | @since 0.1
deriving via (FromNum Int) instance AMonoid Int

-- | @since 0.1
deriving via (FromNum Int8) instance AMonoid Int8

-- | @since 0.1
deriving via (FromNum Int16) instance AMonoid Int16

-- | @since 0.1
deriving via (FromNum Int32) instance AMonoid Int32

-- | @since 0.1
deriving via (FromNum Int64) instance AMonoid Int64

-- | @since 0.1
deriving via (FromNum Integer) instance AMonoid Integer

-- | @since 0.1
deriving via (FromNum Word) instance AMonoid Word

-- | @since 0.1
deriving via (FromNum Word8) instance AMonoid Word8

-- | @since 0.1
deriving via (FromNum Word16) instance AMonoid Word16

-- | @since 0.1
deriving via (FromNum Word32) instance AMonoid Word32

-- | @since 0.1
deriving via (FromNum Word64) instance AMonoid Word64

-- | @since 0.1
deriving via (FromNum Natural) instance AMonoid Natural

-- | @since 0.1
deriving via (FromNum (Ratio Integer)) instance AMonoid (Ratio Integer)

-- | @since 0.1
deriving via (FromNum (Ratio Natural)) instance AMonoid (Ratio Natural)

-- | @since 0.1
deriving via (FromNum (Complex a)) instance (RealFloat a) => AMonoid (Complex a)

-- | @since 0.1
deriving via (FromNum (Fixed k)) instance (HasResolution k) => AMonoid (Fixed k)

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

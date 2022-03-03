-- | Provides the 'AMonoid' typeclass.
--
-- @since 0.1.0.0
module Numeric.Algebra.Additive.AMonoid
  ( AMonoid (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))

-- | Defines a monoid over an additive semigroup.
--
-- @since 0.1.0.0
class ASemigroup m => AMonoid m where
  -- | @since 0.1.0.0
  zero :: m

-- | @since 0.1.0.0
instance AMonoid Double where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Float where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int8 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int16 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int32 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Int64 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Integer where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Natural where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word8 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word16 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word32 where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid Word64 where
  zero = 0

instance AMonoid (Ratio Integer) where
  zero = 0

instance AMonoid (Ratio Natural) where
  zero = 0

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a) where
  zero = (zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a) where
  zero = (zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a) where
  zero = (zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero)

-- | @since 0.1.0.0
instance AMonoid a => AMonoid (a, a, a, a, a, a, a, a, a) where
  zero = (zero, zero, zero, zero, zero, zero, zero, zero, zero)
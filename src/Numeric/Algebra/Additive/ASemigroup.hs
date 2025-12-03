-- | Provides the 'ASemigroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.ASemigroup
  ( ASemigroup (..),
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
import Numeric.Algebra.Deriving
  ( FromFractional (MkFromFractional),
    FromIntegral (MkFromIntegral),
    FromNum (MkFromNum),
  )

-- | Defines an additive semigroup.
--
-- @since 0.1
type ASemigroup :: Type -> Constraint
class ASemigroup s where
  -- | @since 0.1
  (.+.) :: s -> s -> s

infixl 6 .+.

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => ASemigroup (FromFractional a)

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => ASemigroup (FromIntegral a)

-- | @since 0.1
instance (Num a) => ASemigroup (FromNum a) where
  (.+.) = coerce @(a -> a -> a) @(FromNum a -> FromNum a -> FromNum a) (+)
  {-# INLINE (.+.) #-}

-- | @since 0.1
deriving via (FromNum Double) instance ASemigroup Double

-- | @since 0.1
deriving via (FromNum Float) instance ASemigroup Float

-- | @since 0.1
deriving via (FromNum Int) instance ASemigroup Int

-- | @since 0.1
deriving via (FromNum Int8) instance ASemigroup Int8

-- | @since 0.1
deriving via (FromNum Int16) instance ASemigroup Int16

-- | @since 0.1
deriving via (FromNum Int32) instance ASemigroup Int32

-- | @since 0.1
deriving via (FromNum Int64) instance ASemigroup Int64

-- | @since 0.1
deriving via (FromNum Integer) instance ASemigroup Integer

-- | @since 0.1
deriving via (FromNum Word) instance ASemigroup Word

-- | @since 0.1
deriving via (FromNum Word8) instance ASemigroup Word8

-- | @since 0.1
deriving via (FromNum Word16) instance ASemigroup Word16

-- | @since 0.1
deriving via (FromNum Word32) instance ASemigroup Word32

-- | @since 0.1
deriving via (FromNum Word64) instance ASemigroup Word64

-- | @since 0.1
deriving via (FromNum Natural) instance ASemigroup Natural

-- | @since 0.1
deriving via (FromNum (Ratio Integer)) instance ASemigroup (Ratio Integer)

-- | @since 0.1
deriving via (FromNum (Ratio Natural)) instance ASemigroup (Ratio Natural)

-- | @since 0.1
deriving via (FromNum (Complex a)) instance (RealFloat a) => ASemigroup (Complex a)

-- | @since 0.1
deriving via (FromNum (Fixed k)) instance (HasResolution k) => ASemigroup (Fixed k)

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a) where
  (x1, x2) .+. (y1, y2) = (x1 .+. y1, x2 .+. y2)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a) where
  (x1, x2, x3) .+. (y1, y2, y3) = (x1 .+. y1, x2 .+. y2, x3 .+. y3)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a) where
  (x1, x2, x3, x4) .+. (y1, y2, y3, y4) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4
    )
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .+. (y1, y2, y3, y4, y5) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5
    )
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .+. (y1, y2, y3, y4, y5, y6) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6
    )
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7) .+. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7
    )
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7, x8) .+. (y1, y2, y3, y4, y5, y6, y7, y8) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7,
      x8 .+. y8
    )
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (ASemigroup a) => ASemigroup (a, a, a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .+. (y1, y2, y3, y4, y5, y6, y7, y8, y9) =
    ( x1 .+. y1,
      x2 .+. y2,
      x3 .+. y3,
      x4 .+. y4,
      x5 .+. y5,
      x6 .+. y6,
      x7 .+. y7,
      x8 .+. y8,
      x9 .+. y9
    )
  {-# INLINE (.+.) #-}

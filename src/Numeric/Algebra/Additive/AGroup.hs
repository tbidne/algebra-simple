-- | Provides the 'AGroup' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Additive.AGroup
  ( AGroup (..),
    anegate,
  )
where

import Data.Coerce (coerce)
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Deriving
  ( FromFractional (MkFromFractional),
    FromIntegral (MkFromIntegral),
    FromNum (MkFromNum),
  )

-- | Defines an additive group.
--
-- @since 0.1
type AGroup :: Type -> Constraint
class (AMonoid g) => AGroup g where
  -- | @since 0.1
  (.-.) :: g -> g -> g

infixl 6 .-.

-- | @since 0.1
anegate :: (AGroup g) => g -> g
anegate n = zero .-. n
{-# INLINE anegate #-}

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => AGroup (FromFractional a)

-- | @since 0.1
deriving via (FromNum a) instance (Num a) => AGroup (FromIntegral a)

-- | @since 0.1
instance (Num a) => AGroup (FromNum a) where
  (.-.) = coerce @(a -> a -> a) @(FromNum a -> FromNum a -> FromNum a) (-)
  {-# INLINE (.-.) #-}

-- | @since 0.1
deriving via (FromNum Double) instance AGroup Double

-- | @since 0.1
deriving via (FromNum Float) instance AGroup Float

-- | @since 0.1
deriving via (FromNum Int) instance AGroup Int

-- | @since 0.1
deriving via (FromNum Int8) instance AGroup Int8

-- | @since 0.1
deriving via (FromNum Int16) instance AGroup Int16

-- | @since 0.1
deriving via (FromNum Int32) instance AGroup Int32

-- | @since 0.1
deriving via (FromNum Int64) instance AGroup Int64

-- | @since 0.1
deriving via (FromNum Integer) instance AGroup Integer

-- | @since 0.1
deriving via (FromNum Word) instance AGroup Word

-- | @since 0.1
deriving via (FromNum Word8) instance AGroup Word8

-- | @since 0.1
deriving via (FromNum Word16) instance AGroup Word16

-- | @since 0.1
deriving via (FromNum Word32) instance AGroup Word32

-- | @since 0.1
deriving via (FromNum Word64) instance AGroup Word64

-- | @since 0.1
deriving via (FromNum (Ratio Integer)) instance AGroup (Ratio Integer)

-- | @since 0.1
deriving via (FromNum (Complex a)) instance (RealFloat a) => AGroup (Complex a)

-- | @since 0.1
deriving via (FromNum (Fixed k)) instance (HasResolution k) => AGroup (Fixed k)

-- | @since 0.1
instance (AGroup a) => AGroup (a, a) where
  (x1, x2) .-. (y1, y2) = (x1 .-. y1, x2 .-. y2)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a) where
  (x1, x2, x3) .-. (y1, y2, y3) = (x1 .-. y1, x2 .-. y2, x3 .-. y3)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a) where
  (x1, x2, x3, x4) .-. (y1, y2, y3, y4) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a) where
  (x1, x2, x3, x4, x5) .-. (y1, y2, y3, y4, y5) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6) .-. (y1, y2, y3, y4, y5, y6) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7) .-. (y1, y2, y3, y4, y5, y6, y7) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7, x8) .-. (y1, y2, y3, y4, y5, y6, y7, y8) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7,
      x8 .-. y8
    )
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (AGroup a) => AGroup (a, a, a, a, a, a, a, a, a) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) .-. (y1, y2, y3, y4, y5, y6, y7, y8, y9) =
    ( x1 .-. y1,
      x2 .-. y2,
      x3 .-. y3,
      x4 .-. y4,
      x5 .-. y5,
      x6 .-. y6,
      x7 .-. y7,
      x8 .-. y8,
      x9 .-. y9
    )
  {-# INLINE (.-.) #-}

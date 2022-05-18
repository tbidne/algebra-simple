-- | Provides the 'NumLiteral' typeclass.
--
-- @since 0.1
module Numeric.Class.Literal
  ( NumLiteral (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Replaces 'Num'\'s ' 'fromInteger' functionality for when we do not have
-- a 'Num' instance. Instead of, e.g., @1_000 :: Num a => a@ we have
-- @fromLit 1_000 :: NumLiteral a => a@. Unfortunately this is partial for
-- 'Natural' and has overflow issues for finite types.
--
-- @since 0.1
type NumLiteral :: Type -> Constraint
class NumLiteral a where
  -- | @since 0.1
  fromLit :: Integer -> a

-- | @since 0.1
instance NumLiteral Double where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Float where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int8 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int16 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int32 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Int64 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Integer where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance NumLiteral Natural where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word8 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word16 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word32 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral Word64 where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | @since 0.1
instance NumLiteral (Ratio Integer) where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

-- | __WARNING: Partial__
--
-- @since 0.1
instance NumLiteral (Ratio Natural) where
  fromLit = fromInteger
  {-# INLINE fromLit #-}

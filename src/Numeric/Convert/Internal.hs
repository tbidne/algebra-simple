-- | @since 0.1
module Numeric.Convert.Internal
  ( -- * Integers
    FromInteger (..),
    ToInteger (..),

    -- * Rationals
    FromRational (..),
    ToRational (..),

    -- * Reals
    FromReal (..),
    ToReal (..),
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
import GHC.Stack.Types (HasCallStack)
import Numeric.Algebra.Deriving
  ( AsFractional (MkAsFractional),
    AsIntegral (MkAsIntegral),
    AsNum (MkAsNum),
    AsReal (MkAsReal),
  )

-- NOTE: Internal module so that we can give our typeclasses mutual
-- constraints.
--
-- In general, conversions @a -> b@ are intended to be embeddings i.e.
-- well-behaved. This is why e.g. ToInteger requires a ToRational constraint
-- -- if you can embed in Z, you can embed in Q -- and why some instances are
-- missing e.g. fromReal :: Double -> Integer.
--
-- That said, there are still some instances of "bad behavior":
--
-- - Natural instances are partial.
-- - Bounded types may over/underflow.
-- - General floating point issues.

{- HLINT ignore FromInteger "Redundant bracket" -}

-------------------------------------------------------------------------------
----------------------------------- INTEGERS ----------------------------------
-------------------------------------------------------------------------------

-- | Replaces base's @fromInteger@ functionality for when we do not have a
-- 'Num' instance.
--
-- @
-- 1_000 :: Num a => a
--
-- -- becomes
--
-- fromZ 1_000 :: FromInteger a => a
-- @
--
-- Note that @fromInteger@'s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromInteger :: Type -> Constraint
class FromInteger a where
  -- | @since 0.1
  fromZ :: (HasCallStack) => Integer -> a

-- | @since 0.1
instance (Fractional a) => FromInteger (AsFractional a) where
  fromZ = coerce @(Integer -> a) @(Integer -> AsFractional a) fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
instance (Num a) => FromInteger (AsNum a) where
  fromZ = coerce @(Integer -> a) @(Integer -> AsNum a) fromInteger
  {-# INLINE fromZ #-}

-- | @since 0.1
deriving via (AsNum Float) instance FromInteger Float

-- | @since 0.1
deriving via (AsNum Double) instance FromInteger Double

-- | @since 0.1
deriving via (AsNum Int) instance FromInteger Int

-- | @since 0.1
deriving via (AsNum Int8) instance FromInteger Int8

-- | @since 0.1
deriving via (AsNum Int16) instance FromInteger Int16

-- | @since 0.1
deriving via (AsNum Int32) instance FromInteger Int32

-- | @since 0.1
deriving via (AsNum Int64) instance FromInteger Int64

-- | @since 0.1
instance FromInteger Integer where
  fromZ = id
  {-# INLINE fromZ #-}

-- | @since 0.1
deriving via (AsNum Word) instance FromInteger Word

-- | @since 0.1
deriving via (AsNum Word8) instance FromInteger Word8

-- | @since 0.1
deriving via (AsNum Word16) instance FromInteger Word16

-- | @since 0.1
deriving via (AsNum Word32) instance FromInteger Word32

-- | @since 0.1
deriving via (AsNum Word64) instance FromInteger Word64

-- | __WARNING: Partial__
--
-- @since 0.1
deriving via (AsNum Natural) instance FromInteger Natural

-- | @since 0.1
deriving via (AsNum (Ratio Integer)) instance FromInteger (Ratio Integer)

-- | __WARNING: Partial__
--
-- @since 0.1
deriving via (AsNum (Ratio Natural)) instance FromInteger (Ratio Natural)

-- | @since 0.1
deriving via (AsNum (Complex a)) instance (RealFloat a) => FromInteger (Complex a)

-- | @since 0.1
deriving via (AsNum (Fixed a)) instance (HasResolution a) => FromInteger (Fixed a)

-- | Integer embedding.
--
-- @since 0.1
type ToInteger :: Type -> Constraint
class (ToRational a) => ToInteger a where
  -- | @since 0.1
  toZ :: (HasCallStack) => a -> Integer

-- | @since 0.1
instance (Integral a) => ToInteger (AsIntegral a) where
  toZ = coerce @(a -> Integer) @(AsIntegral a -> Integer) toInteger
  {-# INLINE toZ #-}

-- | @since 0.1
deriving via (AsIntegral Int) instance ToInteger Int

-- | @since 0.1
deriving via (AsIntegral Int8) instance ToInteger Int8

-- | @since 0.1
deriving via (AsIntegral Int16) instance ToInteger Int16

-- | @since 0.1
deriving via (AsIntegral Int32) instance ToInteger Int32

-- | @since 0.1
deriving via (AsIntegral Int64) instance ToInteger Int64

-- | @since 0.1
instance ToInteger Integer where
  toZ = id
  {-# INLINE toZ #-}

-- | @since 0.1
deriving via (AsIntegral Word) instance ToInteger Word

-- | @since 0.1
deriving via (AsIntegral Word8) instance ToInteger Word8

-- | @since 0.1
deriving via (AsIntegral Word16) instance ToInteger Word16

-- | @since 0.1
deriving via (AsIntegral Word32) instance ToInteger Word32

-- | @since 0.1
deriving via (AsIntegral Word64) instance ToInteger Word64

-- | @since 0.1
deriving via (AsIntegral Natural) instance ToInteger Natural

-------------------------------------------------------------------------------
---------------------------------- RATIONALS ----------------------------------
-------------------------------------------------------------------------------

-- | Replaces base's @fromRational@ functionality for when we do not have a
-- 'Fractional' instance.
--
-- @
-- 5.5 :: Fractional a => a
--
-- -- becomes
--
-- fromQ 5.5 :: FromRational a => a
-- @
--
-- Note that @fromRational@'s deficiencies are inherited e.g. 'Natural' is
-- partial, bounded types have over/underflow issues.
--
-- @since 0.1
type FromRational :: Type -> Constraint
class (FromInteger a) => FromRational a where
  -- | @since 0.1
  fromQ :: (HasCallStack) => Rational -> a

-- | @since 0.1
instance (Fractional a) => FromRational (AsFractional a) where
  fromQ = coerce @(Rational -> a) @(Rational -> AsFractional a) fromRational
  {-# INLINE fromQ #-}

-- | @since 0.1
deriving via (AsFractional Float) instance FromRational Float

-- | @since 0.1
deriving via (AsFractional Double) instance FromRational Double

-- | @since 0.1
instance FromRational (Ratio Integer) where
  fromQ = id
  {-# INLINE fromQ #-}

-- | __WARNING: Partial__
--
-- @since 0.1
deriving via (AsFractional (Ratio Natural)) instance FromRational (Ratio Natural)

-- | @since 0.1
deriving via (AsFractional (Complex a)) instance (RealFloat a) => FromRational (Complex a)

-- | @since 0.1
deriving via (AsFractional (Fixed k)) instance (HasResolution k) => FromRational (Fixed k)

-- | Rational embedding.
--
-- @since 0.1
type ToRational :: Type -> Constraint
class (ToReal a) => ToRational a where
  -- | @since 0.1
  toQ :: (HasCallStack) => a -> Rational

-- | @since 0.1
instance (Real a) => ToRational (AsReal a) where
  toQ = coerce @(a -> Rational) @(AsReal a -> Rational) toRational
  {-# INLINE toQ #-}

-- | @since 0.1
instance (Integral a) => ToRational (AsIntegral a) where
  toQ = coerce @(a -> Rational) @(AsIntegral a -> Rational) toRational
  {-# INLINE toQ #-}

-- | @since 0.1
deriving via (AsReal Float) instance ToRational Float

-- | @since 0.1
deriving via (AsReal Double) instance ToRational Double

-- | @since 0.1
deriving via (AsReal Int) instance ToRational Int

-- | @since 0.1
deriving via (AsReal Int8) instance ToRational Int8

-- | @since 0.1
deriving via (AsReal Int16) instance ToRational Int16

-- | @since 0.1
deriving via (AsReal Int32) instance ToRational Int32

-- | @since 0.1
deriving via (AsReal Int64) instance ToRational Int64

-- | @since 0.1
deriving via (AsReal Integer) instance ToRational Integer

-- | @since 0.1
deriving via (AsReal Word) instance ToRational Word

-- | @since 0.1
deriving via (AsReal Word8) instance ToRational Word8

-- | @since 0.1
deriving via (AsReal Word16) instance ToRational Word16

-- | @since 0.1
deriving via (AsReal Word32) instance ToRational Word32

-- | @since 0.1
deriving via (AsReal Word64) instance ToRational Word64

-- | @since 0.1
deriving via (AsReal Natural) instance ToRational Natural

-- | @since 0.1
instance ToRational (Ratio Integer) where
  toQ = id
  {-# INLINE toQ #-}

-- | @since 0.1
deriving via (AsReal (Ratio Natural)) instance ToRational (Ratio Natural)

-- | @since 0.1
deriving via (AsReal (Fixed k)) instance (HasResolution k) => ToRational (Fixed k)

-------------------------------------------------------------------------------
------------------------------------- REALS -----------------------------------
-------------------------------------------------------------------------------

-- NOTE: [Derived classes choice]
--
-- In general, we choose which AsX class to use based on what we need for
-- the default instance. For example, for FromReal we use
--
--     realToFrac :: (Real a, Fractional b) => a -> b
--     realToFrac :: (Fractional b) => Double -> b
--
-- hence this is based on the Fractional constraint. We sometimes implement
-- instances for multiple AsX classes when another AsX class needs the
-- constraint in order to be used elsewhere e.g. we have
--
--     ToRational (AsIntegral a)
--
-- because we need it for
--
--     ToInteger (AsIntegral a)
--
-- which we use for deriving. Otherwise, we do not include any other instances.

-- | Conversion from 'Double'.
--
-- @since 0.1
type FromReal :: Type -> Constraint
class (FromRational a) => FromReal a where
  -- | @since 0.1
  fromR :: (HasCallStack) => Double -> a

-- | @since 0.1
instance (Fractional a) => FromReal (AsFractional a) where
  fromR = coerce @(Double -> a) @(Double -> AsFractional a) realToFrac
  {-# INLINE fromR #-}

-- | @since 0.1
deriving via (AsFractional Float) instance FromReal Float

-- | @since 0.1
instance FromReal Double where
  fromR = id
  {-# INLINE fromR #-}

-- | @since 0.1
deriving via (AsFractional (Ratio Integer)) instance FromReal (Ratio Integer)

-- | __WARNING: Partial__
--
-- @since 0.1
deriving via (AsFractional (Ratio Natural)) instance FromReal (Ratio Natural)

-- | @since 0.1
deriving via (AsFractional (Complex a)) instance (RealFloat a) => FromReal (Complex a)

-- | @since 0.1
deriving via (AsFractional (Fixed k)) instance (HasResolution k) => FromReal (Fixed k)

-- | Conversion to Double.
--
-- @since 0.1
type ToReal :: Type -> Constraint
class ToReal a where
  -- | @since 0.1
  toR :: (HasCallStack) => a -> Double

instance (Integral a) => ToReal (AsIntegral a) where
  toR = coerce @(a -> Double) @(AsIntegral a -> Double) realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
instance (Real a) => ToReal (AsReal a) where
  toR = coerce @(a -> Double) @(AsReal a -> Double) realToFrac
  {-# INLINE toR #-}

-- | @since 0.1
deriving via (AsReal Float) instance ToReal Float

-- | @since 0.1
instance ToReal Double where
  toR = id
  {-# INLINE toR #-}

-- | @since 0.1
deriving via (AsReal Int) instance ToReal Int

-- | @since 0.1
deriving via (AsReal Int8) instance ToReal Int8

-- | @since 0.1
deriving via (AsReal Int16) instance ToReal Int16

-- | @since 0.1
deriving via (AsReal Int32) instance ToReal Int32

-- | @since 0.1
deriving via (AsReal Int64) instance ToReal Int64

-- | @since 0.1
deriving via (AsReal Integer) instance ToReal Integer

-- | @since 0.1
deriving via (AsReal Word) instance ToReal Word

-- | @since 0.1
deriving via (AsReal Word8) instance ToReal Word8

-- | @since 0.1
deriving via (AsReal Word16) instance ToReal Word16

-- | @since 0.1
deriving via (AsReal Word32) instance ToReal Word32

-- | @since 0.1
deriving via (AsReal Word64) instance ToReal Word64

-- | @since 0.1
deriving via (AsReal Natural) instance ToReal Natural

-- | @since 0.1
deriving via (AsReal (Ratio Integer)) instance ToReal (Ratio Integer)

-- | @since 0.1
deriving via (AsReal (Ratio Natural)) instance ToReal (Ratio Natural)

-- | @since 0.1
deriving via (AsReal (Fixed k)) instance (HasResolution k) => ToReal (Fixed k)

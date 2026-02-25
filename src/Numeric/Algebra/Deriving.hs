-- | Provides deriving utilities.
--
-- @since 0.1
module Numeric.Algebra.Deriving
  ( AsNum (..),
    AsFractional (..),
    AsIntegral (..),
    AsReal (..),
  )
where

-- | Derives classes from 'Fractional' constraint.
--
-- @since 0.1
newtype AsFractional a = MkAsFractional a

-- | Derives classes from 'Integral' constraint.
--
-- @since 0.1
newtype AsIntegral a = MkAsIntegral a

-- | Derives classes from 'Num' constraint.
--
-- @since 0.1
newtype AsNum a = MkAsNum a

-- | Derives classes from 'Real' constraint.
--
-- @since 0.1
newtype AsReal a = MkAsReal a

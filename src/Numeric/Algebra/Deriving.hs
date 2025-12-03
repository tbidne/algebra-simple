-- | Provides deriving utilities.
--
-- @since 0.1
module Numeric.Algebra.Deriving
  ( FromNum (..),
    FromFractional (..),
    FromIntegral (..),
  )
where

-- | Derives classes from 'Fractional' constraint.
--
-- @since 0.1
newtype FromFractional a = MkFromFractional a

-- | Derives classes from 'Integral' constraint.
--
-- @since 0.1
newtype FromIntegral a = MkFromIntegral a

-- | Derives classes from 'Num' constraint.
--
-- @since 0.1
newtype FromNum a = MkFromNum a

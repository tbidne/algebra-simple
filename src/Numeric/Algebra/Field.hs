-- | Provides the 'Field' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Field
  ( Field,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Multiplicative.MGroup (MGroup (..))
import Numeric.Algebra.Ring (Ring)

-- | Defines a field.
--
-- @since 0.1
class (Ring f, MGroup f) => Field f

-- | @since 0.1
instance Field Float

-- | @since 0.1
instance Field Double

-- | @since 0.1
instance Field Int

-- | @since 0.1
instance Field Int8

-- | @since 0.1
instance Field Int16

-- | @since 0.1
instance Field Int32

-- | @since 0.1
instance Field Int64

-- | @since 0.1
instance Field Integer

-- | @since 0.1
instance Field Word

-- | @since 0.1
instance Field Word8

-- | @since 0.1
instance Field Word16

-- | @since 0.1
instance Field Word32

-- | @since 0.1
instance Field Word64

-- | @since 0.1
instance Field (Ratio Integer)

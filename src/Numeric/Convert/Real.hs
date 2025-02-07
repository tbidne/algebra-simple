-- | Provides the 'Internal.FromReal' and 'Internal.ToReal' typeclasses.
--
-- @since 0.1
module Numeric.Convert.Real
  ( Internal.FromReal (..),
    Internal.ToReal (..),

    -- * Aliases
    ℝ,
    Fromℝ,
    fromℝ,
    Toℝ,
    toℝ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Double', with U+211D.
--
-- @since 0.1
type ℝ = Double

-- | Unicode alias for 'Internal.FromReal', with U+211D.
--
-- @since 0.1
type Fromℝ = Internal.FromReal

-- | Unicode alias for 'Internal.fromR', with U+211D.
--
-- @since 0.1
fromℝ :: (Fromℝ a, HasCallStack) => ℝ -> a
fromℝ = Internal.fromR

-- | Unicode alias for 'Internal.ToReal', with U+211D.
--
-- @since 0.1
type Toℝ = Internal.ToReal

-- | Unicode alias for 'Internal.toR', with U+211D.
--
-- @since 0.1
toℝ :: (HasCallStack, Toℝ a) => a -> ℝ
toℝ = Internal.toR

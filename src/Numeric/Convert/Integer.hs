-- | Provides the 'Internal.FromInteger' and 'Internal.ToInteger'
-- typeclasses.
--
-- @since 0.1
module Numeric.Convert.Integer
  ( Internal.FromInteger (..),
    Internal.ToInteger (..),

    -- * Aliases
    ℤ,
    Fromℤ,
    fromℤ,
    Toℤ,
    toℤ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Integer', with U+2114.
--
-- @since 0.1
type ℤ = Integer

-- | Unicode alias for 'Internal.FromInteger', with U+2114.
--
-- @since 0.1
type Fromℤ = Internal.FromInteger

-- | Unicode alias for 'Internal.fromZ', with U+2114.
--
-- @since 0.1
fromℤ :: (Fromℤ a, HasCallStack) => ℤ -> a
fromℤ = Internal.fromZ

-- | Unicode alias for 'Internal.ToInteger', with U+2114.
--
-- @since 0.1
type Toℤ = Internal.ToInteger

-- | Unicode alias for 'Internal.toZ', with U+2114.
--
-- @since 0.1
toℤ :: (HasCallStack, Toℤ a) => a -> ℤ
toℤ = Internal.toZ

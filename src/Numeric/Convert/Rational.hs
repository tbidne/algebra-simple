-- | Provides the 'Internal.FromRational' and 'Internal.ToRational
-- typeclasses.
--
-- @since 0.1
module Numeric.Convert.Rational
  ( Internal.FromRational (..),
    Internal.ToRational (..),

    -- * Aliases
    ℚ,
    Fromℚ,
    fromℚ,
    Toℚ,
    toℚ,
  )
where

import GHC.Stack (HasCallStack)
import Numeric.Convert.Internal qualified as Internal

-- | Unicode alias for 'Rational', with U+211A.
--
-- @since 0.1
type ℚ = Rational

-- | Unicode alias for 'Internal.FromRational', with U+211A.
--
-- @since 0.1
type Fromℚ = Internal.FromRational

-- | Unicode alias for 'Internal.fromQ', with U+211A.
--
-- @since 0.1
fromℚ :: (Fromℚ a, HasCallStack) => ℚ -> a
fromℚ = Internal.fromQ

-- | Unicode alias for 'Internal.ToRational', with U+211A.
--
-- @since 0.1
type Toℚ = Internal.ToRational

-- | Unicode alias for 'Internal.toQ', with U+211A.
--
-- @since 0.1
toℚ :: (HasCallStack, Toℚ a) => a -> ℚ
toℚ = Internal.toQ

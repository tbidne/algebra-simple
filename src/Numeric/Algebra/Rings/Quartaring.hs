-- | Provides the 'Quartaring' typeclass.
--
-- @since 0.1
module Numeric.Algebra.Rings.Quartaring
  ( Quartaring,
  )
where

import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Algebra.Additive.ASemigroup (ASemigroup)
import Numeric.Algebra.Multiplicative.MSemigroup (MSemigroup)
import Numeric.Natural (Natural)

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup ((.+.))
-- >>> import Numeric.Algebra.Multiplicative.MSemigroup ((.*.))

-- | Defines a 'Quartaring' i.e. a structure with addition and multiplication.
-- In other words, a semiring that has no identity requirement. Should satisfy:
--
-- * Distributive property:
--
--     @
--     a .*. (b .+. c) === (a .*. b) .+. (a .*. c)
--     (b .+. c) .*. a === (b .*. a) .+. (c .*. a)
--     @
--
-- ==== __Examples:__
--
-- - \( \mathbb{Z}^{\gt 1} \), the integers greater than one.
--
-- >>> :{
--   -- Addition
--   f1 :: (Quartaring r) => r -> r
--   f1 x = x .+. x
-- :}
--
-- >>> f1 5
-- 10
--
-- >>> :{
--   -- Multiplication
--   f2 :: (Quartaring r) => r -> r
--   f2 x = x .*. x
-- :}
--
-- >>> f2 5
-- 25
--
-- @since 0.1
type Quartaring :: Type -> Constraint
class (ASemigroup r, MSemigroup r) => Quartaring r

-- | @since 0.1
instance Quartaring Double

-- | @since 0.1
instance Quartaring Float

-- | @since 0.1
instance Quartaring Int

-- | @since 0.1
instance Quartaring Int8

-- | @since 0.1
instance Quartaring Int16

-- | @since 0.1
instance Quartaring Int32

-- | @since 0.1
instance Quartaring Int64

-- | @since 0.1
instance Quartaring Integer

-- | @since 0.1
instance Quartaring Natural

-- | @since 0.1
instance Quartaring Word

-- | @since 0.1
instance Quartaring Word8

-- | @since 0.1
instance Quartaring Word16

-- | @since 0.1
instance Quartaring Word32

-- | @since 0.1
instance Quartaring Word64

-- | @since 0.1
instance Quartaring (Ratio Integer)

-- | @since 0.1
instance Quartaring (Ratio Natural)

-- | @since 0.1
instance (RealFloat a) => Quartaring (Complex a)

-- | @since 0.1
instance (HasResolution k) => Quartaring (Fixed k)

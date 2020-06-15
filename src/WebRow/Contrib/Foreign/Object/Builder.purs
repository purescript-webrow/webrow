module WebRow.Contrib.Foreign.Object.Builder where

import Prelude

import Foreign.Object (Object)
import Record.Unsafe (unsafeSet) as Record.Unsafe
import Unsafe.Coerce (unsafeCoerce)

newtype Builder a = Builder (Object a → Object a)

instance semigroupBuilder ∷ Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)

instance monoidBuilder ∷ Monoid (Builder a) where
  mempty = Builder identity

insert ∷ ∀ a. String → a → Builder a
insert k v = unsafeCoerce (Record.Unsafe.unsafeSet k v)

build ∷ ∀ a. Builder a → Object a
build (Builder b) = b (unsafeCoerce {})

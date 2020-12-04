module WebRow.Contrib.Data.Variant where

import Prelude

import Data.Symbol (reflectSymbol)
import Data.Variant (Unvariant(..), Variant, on, unvariant)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy)
import Unsafe.Coerce (unsafeCoerce)

tag ∷ ∀ r. Variant r → String
tag v =
  let
    Unvariant f = unvariant v
  in
    f (\c _ → reflectSymbol c)

override
  ∷ ∀ sym a b r1 r2
  . Row.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → (a → b)
  → (Variant r2 → b)
  → Variant r2
  → b
override p f g = (g <<< expand) # on p f
  where
    expand ∷ Variant r1 → Variant r2
    expand = unsafeCoerce

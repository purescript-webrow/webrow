module WebRow.Contrib.Data.Variant where

import Data.Symbol (reflectSymbol)
import Data.Variant (Unvariant(..), Variant, unvariant)

tag ∷ ∀ r. Variant r → String
tag v =
  let
    Unvariant f = unvariant v
  in
    f (\c _ → reflectSymbol c)

module WebRow.Contrib.Run where

import Run (EFFECT, AFF)
import Type.Prelude (SProxy(..))

_effect = SProxy ∷ SProxy "effect"

type EffRow eff
  = ( effect ∷ EFFECT | eff )

_aff = SProxy ∷ SProxy "aff"

type AffRow eff
  = ( aff ∷ AFF | eff )

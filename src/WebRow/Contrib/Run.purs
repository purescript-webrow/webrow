module WebRow.Contrib.Run where

import Run (EFFECT, AFF)

type EffRow eff
  = ( effect ∷ EFFECT | eff )

type AffRow eff
  = ( aff ∷ AFF | eff )

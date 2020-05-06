module WebRow.Registration.Interpret.Dummy where

import Prelude

import Effect.Random (random)
import Run (EFFECT, Run, liftEffect)
import Run (interpret, on, send) as Run
import WebRow.Applets.Registration.Effects (RegisterF(..), REGISTER)
import WebRow.Applets.Registration.Types (_register)

interpret
  ∷ ∀ eff
  . Run
    ( effect ∷ EFFECT
    , register ∷ REGISTER
    | eff
    )
   ~> Run (effect ∷ EFFECT | eff)
interpret = Run.interpret (Run.on _register interpreter Run.send)

interpreter
  ∷ ∀ eff
  . RegisterF ~> Run (effect ∷ EFFECT | eff)
interpreter (EmailTaken email next) = do
  v ← liftEffect random
  pure (next (v > 0.5))



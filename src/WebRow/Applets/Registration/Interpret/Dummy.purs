module WebRow.Registration.Interpret.Dummy where

import Prelude

import Effect.Random (random)
import Run (Run, liftEffect)
import Run (interpret, on, send) as Run
import Type.Row (type (+))
import WebRow.Applets.Registration.Effects (Registration, RegistrationF(..), _registration)
import WebRow.Types (Effect)

interpret
  ∷ ∀ eff
  . Run
    ( Registration
    + Effect
    + eff
    )
   ~> Run (Effect + eff)
interpret = Run.interpret (Run.on _registration handler Run.send)

handler
  ∷ ∀ eff
  . RegistrationF ~> Run (Effect + eff)
handler (EmailTaken email next) = do
  v ← liftEffect random
  pure (next (v > 0.5))



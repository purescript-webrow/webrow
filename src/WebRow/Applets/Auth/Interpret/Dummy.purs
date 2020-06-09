module WebRow.Auth.Interpret.Dummy where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Random (random)
import Run (EFFECT, Run, liftEffect)
import Run (interpret, on, send) as Run
import WebRow.Applets.Auth.Effects (AUTH, AuthF(..))
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Mailer (Email(..))

interpret
  ∷ ∀ eff
  . Run
    ( effect ∷ EFFECT
    , auth ∷ AUTH ()
    | eff
    )
   ~> Run (effect ∷ EFFECT | eff)
interpret = Run.interpret (Run.on _auth handler Run.send)

handler
  ∷ ∀ eff
  . AuthF () ~> Run (effect ∷ EFFECT | eff)
handler = case _ of
  CurrentUser next → do
    pure $ next (Just { email: Email "user@example.com" })
  CheckPassword email user next → do
    v ← liftEffect random
    pure (next (v > 0.5))




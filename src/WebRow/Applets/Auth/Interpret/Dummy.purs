module WebRow.Auth.Interpret.Dummy where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Random (random)
import Run (EFFECT, Run, liftEffect)
import Run (interpret, on, send) as Run
import WebRow.Applets.Auth.Effects (AUTH, AuthF(..))
import WebRow.Applets.Auth.Types (_auth)

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
handler (Authenticate email password next) = do
  v ← liftEffect random
  pure $ next (Just { email })


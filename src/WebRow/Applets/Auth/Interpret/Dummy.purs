module WebRow.Applets.Auth.Interpret.Dummy where

import Prelude

import Data.Maybe (Maybe(..))
import Run (EFFECT, Run)
import Run (interpret, on, send) as Run
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (AUTH, Auth(..))
import WebRow.Applets.Auth.Types (_auth)

interpret ∷
  ∀ eff.
  Run
    ( EFFECT
    + AUTH ()
    + eff
    )
    ~> Run ( EFFECT + eff )
interpret = Run.interpret (Run.on _auth handler Run.send)

handler ∷
  ∀ eff.
  Auth () ~> Run ( EFFECT + eff )
handler (Authenticate email _ next) = do
  pure $ next (Just { email })

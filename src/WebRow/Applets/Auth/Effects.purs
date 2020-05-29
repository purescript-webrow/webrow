module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (FProxy, Run)
import Run (lift) as Run
import WebRow.Applets.Auth.Types (_auth, Password)
import WebRow.Mailer (Email)

data AuthF a = Authenticate Email Password (Maybe Unit → a)

derive instance functorAuthF ∷ Functor (AuthF)

type AUTH = FProxy (AuthF)

authenticate
  ∷ ∀ eff
  . Email
  → Password
  → Run ( auth ∷ AUTH | eff ) (Maybe Unit)
authenticate email password = Run.lift _auth (Authenticate email password identity)

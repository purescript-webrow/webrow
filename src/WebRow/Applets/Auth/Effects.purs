module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (FProxy, Run)
import Run (lift) as Run
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration.Types (Password)
import WebRow.Mailer (Email)

data AuthF user a = Authenticate Email Password (Maybe user → a)

derive instance functorAuthF ∷ Functor (AuthF user)

type AUTH user = FProxy (AuthF user)

authenticate
  ∷ ∀ user eff
  . Email
  → Password
  → Run ( auth ∷ AUTH user | eff ) (Maybe user)
authenticate email password = Run.lift _auth (Authenticate email password identity)

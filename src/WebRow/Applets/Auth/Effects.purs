module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (FProxy, Run)
import Run (lift) as Run
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Mailer (Email)

type User user = { email ∷ Email | user }

data AuthF user a = CurrentUser (Maybe (User user) → a)

derive instance functorAuthF ∷ Functor (AuthF user)

type AUTH user = FProxy (AuthF user)

currentUser ∷ ∀ eff user. Run ( auth ∷ AUTH user | eff ) (Maybe (User user))
currentUser = Run.lift _auth (CurrentUser identity)



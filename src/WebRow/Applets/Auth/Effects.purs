module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (FProxy, Run)
import Run (lift) as Run
import WebRow.Applets.Auth.Types (Password, _auth)
import WebRow.Mailer (Email)

data AuthF user a
  = Authenticate Email Password (Maybe (User user) → a)

derive instance functorAuthF ∷ Functor (AuthF user)

type AUTH user = FProxy (AuthF user)

type Auth user r = (auth ∷ AUTH user | r)

type User user = { email ∷ Email | user }

authenticate
  ∷ ∀ eff user
  . Email
  → Password
  → Run ( auth ∷ AUTH user | eff ) (Maybe (User user))
authenticate email password = Run.lift _auth (Authenticate email password identity)


-- data AuthF user a
--   = CurrentUser (Maybe (User user) → a)
--   | CheckPassword Email Password (Boolean → a)
-- 
-- derive instance functorAuthF ∷ Functor (AuthF user)
-- 
-- type AUTH user = FProxy (AuthF user)
-- 
-- currentUser ∷ ∀ eff user. Run ( auth ∷ AUTH user | eff ) (Maybe (User user))
-- currentUser = Run.lift _auth (CurrentUser identity)
-- 
-- checkPassword ∷ ∀ eff user. Email → Password → Run ( auth ∷ AUTH user | eff ) Boolean
-- checkPassword email password = Run.lift _auth (CheckPassword email password identity)


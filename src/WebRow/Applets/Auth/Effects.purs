module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (Run)
import Run (lift) as Run
import Type.Row (type (+))
import WebRow.Applets.Auth.Types (Password, _auth)
import WebRow.Mailer (Email)

data Auth user a
  = Authenticate Email Password (Maybe (User user) → a)

derive instance functorAuth ∷ Functor (Auth user)

type AUTH user r
  = ( auth ∷ Auth user | r )

type User user
  = { email ∷ Email | user }

authenticate ∷
  ∀ eff user.
  Email →
  Password →
  Run ( AUTH user + eff ) (Maybe (User user))
authenticate email password = Run.lift _auth (Authenticate email password identity)

-- data Auth user a
--   = CurrentUser (Maybe (User user) → a)
--   | CheckPassword Email Password (Boolean → a)
-- 
-- derive instance functorAuth ∷ Functor (Auth user)
-- 
-- type AUTH user = Proxy (Auth user)
-- 
-- currentUser ∷ ∀ eff user. Run ( auth ∷ AUTH user | eff ) (Maybe (User user))
-- currentUser = Run.lift _auth (CurrentUser identity)
-- 
-- checkPassword ∷ ∀ eff user. Email → Password → Run ( auth ∷ AUTH user | eff ) Boolean
-- checkPassword email password = Run.lift _auth (CheckPassword email password identity)

module WebRow.Applets.Auth.Effects where

import Prelude

import Data.Maybe (Maybe)
import Run (FProxy, Run)
import Run (lift) as Run
-- <<<<<<< HEAD
-- import WebRow.Applets.Auth.Types (Password, _auth)
-- import WebRow.Mailer (Email)
-- 
-- type User user = { email ∷ Email | user }
-- 
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
-- =======
-- import WebRow.Applets.Auth.Types (_auth, Password)
-- import WebRow.Mailer (Email)
-- 
-- data AuthF a = Authenticate Email Password (Maybe Unit → a)
-- 
-- derive instance functorAuthF ∷ Functor (AuthF)
-- 
-- type AUTH = FProxy (AuthF)
-- 
-- authenticate
--   ∷ ∀ eff
--   . Email
--   → Password
--   → Run ( auth ∷ AUTH | eff ) (Maybe Unit)
-- authenticate email password = Run.lift _auth (Authenticate email password identity)
-- >>>>>>> origin/auth-applet

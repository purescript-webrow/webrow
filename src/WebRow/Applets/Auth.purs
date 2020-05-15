module WebRow.Applets.Auth where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Variant (on) as Variant
import Run (Run)
import WebRow.Applets.Auth.Effects (AUTH, User)
import WebRow.Applets.Auth.Effects (currentUser) as Effects
import WebRow.Applets.Auth.Routes as Routes
import WebRow.Applets.Auth.Types (_auth, namespace)
import WebRow.Response (RESPONSE, badRequest')
import WebRow.Response (redirect) as Response
import WebRow.Route (ROUTE)
import WebRow.Route (RelativeUrl(..), printRoute) as Route

userRequired
  ∷ ∀ eff res route user
  . Run
      (auth ∷ AUTH user, response ∷ RESPONSE res, route ∷ ROUTE (Routes.RouteRow route) | eff)
      (User user)
userRequired = Effects.currentUser >>= case _ of
  Just user → pure user
  Nothing → do
    Route.printRoute (namespace Routes.Login) >>= un Route.RelativeUrl >>> Response.redirect

-- router
--   ∷ ∀ a eff ctx res routes user
--   . (Variant routes → Run (Effects ctx res routes user eff) a)
--   → Variant (Auth.Routes.RouteRow + routes)
--   → Run (Effects ctx res routes user eff) a
router = Variant.on _auth case _ of
  Routes.Login → badRequest' "Auth.login not implemented yet"

-- onLoginRoute = request >>= _.method >>> case _ of
--   HTTPure.Post → do
--     body ← fromBody
--     Forms.Plain.run emailPassordForm body >>= case _ of
--       Left { layout } → do
--         Responses.loginFormValidationFailed layout
--       Right { result: { email, password }, layout } → do
--         Effects.authenticate (Email email) (Password password) >>= case _ of
--           Nothing → Responses.emailPasswordMismatch layout
--           Just user → 

module WebRow.Applets.Auth where


import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (on)
import HTTPure as HTTPure
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, User)
import WebRow.Applets.Auth.Forms (loginForm)
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(LoginResponse))
import WebRow.Applets.Auth.Routes as Routes
import WebRow.Applets.Auth.Types (Password, _auth)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.Forms.Widgets (TextInputProps)
import WebRow.HTTPError (HttpError, methodNotAllowed')
import WebRow.Mailer (Email)
import WebRow.Message (Message)
import WebRow.Request (Request)
import WebRow.Request (method) as Request
import WebRow.Session (Session)
import WebRow.Session (modify) as Session

-- router
--   ∷ ∀ a res ctx session eff routes
--   . (Variant routes → Run (Effects session ctx res eff) a)
--   → Variant ( auth ∷ Routes.Route | routes )
--   → Run (Effects session ctx res eff) a
router = on _auth case _ of
  Routes.Login → onLoginRoute

onLoginRoute ∷ ∀ eff messages session user widgets.
   Run
    ( Auth user
    + HttpError
    + Message
      ( authFailed ∷ { email ∷ Email, password ∷ Password }
      , invalidEmailFormat ∷ String
      , singleValueExpected ∷ Maybe (Array String)
      | messages
      )
    + Request
    + Session { user ∷ Maybe (User user) | session }
    + eff
    )
    (Response (textInput ∷ TextInputProps | widgets))
onLoginRoute = Request.method >>= case _ of
  HTTPure.Post → do
    body ← fromBody
    Forms.Uni.validate loginForm body >>= case _ of
      Tuple formLayout Nothing →
        pure $ LoginResponse (LoginFormValidationFailed formLayout)
      Tuple formLayout (Just user) → do
        Session.modify \s → s { user = Just user }
        pure $ LoginResponse LoginSuccess
  HTTPure.Get → do
    form ← Forms.Uni.default loginForm
    pure $ LoginResponse (InitialEmailPassordForm form)
  method → methodNotAllowed'

-- type Effects session ctx res eff user widgets =
--   ( auth ∷ AUTH user
--   -- , session ∷ SESSION { user ∷ Maybe Email | session }
--   | eff
--   )
-- import Prelude
-- 
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (un)
-- import Data.Variant (on) as Variant
-- import Run (Run)
-- -- import WebRow.Applets.Auth.Effects (AUTH, User)
-- -- import WebRow.Applets.Auth.Effects (currentUser) as Effects
-- import WebRow.Applets.Auth.Routes as Routes
-- import WebRow.Applets.Auth.Types (_auth, namespace)
-- import WebRow.Response (RESPONSE, badRequest')
-- import WebRow.Response (redirect) as Response
-- import WebRow.Route (ROUTE)
-- import WebRow.Route (RelativeUrl(..), printRoute) as Route
-- 
-- userRequired
--   ∷ ∀ eff res route user
--   . Run
--       (auth ∷ AUTH user, response ∷ RESPONSE res, route ∷ ROUTE (Routes.RouteRow route) | eff)
--       (User user)
-- userRequired = Effects.currentUser >>= case _ of
--   Just user → pure user
--   Nothing → do
--     Route.printRoute (namespace Routes.Login) >>= un Route.RelativeUrl >>> Response.redirect
-- 
-- -- router
-- --   ∷ ∀ a eff ctx res routes user
-- --   . (Variant routes → Run (Effects ctx res routes user eff) a)
-- --   → Variant (Auth.Routes.RouteRow + routes)
-- --   → Run (Effects ctx res routes user eff) a
-- router = Variant.on _auth case _ of
--   Routes.Login → badRequest' "Auth.login not implemented yet"
-- 
-- -- onLoginRoute = request >>= _.method >>> case _ of
-- --   HTTPure.Post → do
-- --     body ← fromBody
-- --     Forms.Plain.run emailPassordForm body >>= case _ of
-- --       Left { layout } → do
-- --         Responses.loginFormValidationFailed layout
-- --       Right { result: { email, password }, layout } → do
-- --         Effects.authenticate (Email email) (Password password) >>= case _ of
-- --           Nothing → Responses.emailPasswordMismatch layout
-- --           Just user → 

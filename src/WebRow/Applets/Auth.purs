module WebRow.Applets.Auth
  ( module Exports
  , router
  , withUserRequired
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (on)
import HTTPure as HTTPure
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, User)
import WebRow.Applets.Auth.Forms (loginForm)
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..))
import WebRow.Applets.Auth.Routes (routeDuplex, Route(..), RouteRow) as Exports
import WebRow.Applets.Auth.Routes (Route(..)) as Routes
import WebRow.Applets.Auth.Routes (RouteRow)
import WebRow.Applets.Auth.Types (Password, _auth, namespace)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.HTTP (methodNotAllowed', method, redirect)
import WebRow.Mailer (Email)
import WebRow.Routing (fromRelativeUrl)
import WebRow.Routing (printRoute) as Routing
import WebRow.Session (fetch, modify) as Session
import WebRow.Types (WebRow)

type AuthRow messages routes session user eff =
  ( WebRow
    ( authFailed ∷ { email ∷ Email, password ∷ Password }
    , invalidEmailFormat ∷ String
    , singleValueExpected ∷ Maybe (Array String)
    | messages
    )
    { user ∷ Maybe (User user) | session }
    (RouteRow routes)
  + Auth user
  + eff
  )

-- router
--   ∷ ∀ eff messages routes session user
--   . (Variant routes → Run (AuthRow messages session user eff) Response)
--   → Variant ( auth ∷ Routes.Route | routes )
--   → Run (AuthRow messages session user eff) Response
router = on _auth case _ of
  Routes.Login → login
  Routes.Logout → logout

login
  ∷ ∀ eff messages routes session user
  . Run
    ( AuthRow
      messages
      routes
      session
      user
    + eff
    )
    Response
login = method >>= case _ of
  HTTPure.Post → do
    body ← fromBody
    Forms.Uni.validate loginForm body >>= case _ of
      Tuple formLayout Nothing →
        pure $ LoginResponse (LoginFormValidationFailed formLayout)
      Tuple formLayout (Just user) → do
        Session.modify _{ user = Just user }
        pure $ LoginResponse LoginSuccess
  HTTPure.Get → do
    form ← Forms.Uni.default loginForm
    pure $ LoginResponse (InitialEmailPassordForm form)
  method → methodNotAllowed'

logout
  ∷ ∀ eff messages routes session user
  . Run
    ( AuthRow
      messages
      routes
      session
      user
    + eff
    )
    Response
logout = do
  Session.modify _{ user = Nothing }
  pure $ LogoutResponse

withUserRequired ∷ ∀ a eff messages routes session user
  . (User user → Run (AuthRow messages routes session user + eff) a)
  -> Run (AuthRow messages routes session user + eff) a
withUserRequired f = Session.fetch >>= _.user >>> case _ of
  Just user → f user
  Nothing → do
    relativeUrl ← Routing.printRoute (namespace Routes.Login)
    redirect (fromRelativeUrl relativeUrl)



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

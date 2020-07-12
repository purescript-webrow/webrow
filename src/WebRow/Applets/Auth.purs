module WebRow.Applets.Auth
  ( module Exports
  , localRouter
  , router
  , withUserRequired
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on)
import HTTPure as HTTPure
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, User)
import WebRow.Applets.Auth.Forms (loginForm)
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..))
import WebRow.Applets.Auth.Routes (Route(..)) as Routes
import WebRow.Applets.Auth.Routes (Route, RouteRow)
import WebRow.Applets.Auth.Routes (localDuplex, routeBuilder, Route(..), RouteRow) as Exports
import WebRow.Applets.Auth.Types (Password, Namespace, _auth, namespace)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.HTTP (methodNotAllowed', method, redirect)
import WebRow.Mailer (Email)
import WebRow.Routing (fromRelativeUrl)
import WebRow.Routing (printRoute) as Routing
import WebRow.Session (delete, fetch, modify) as Session
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

router
  ∷ ∀ eff messages responses routes session user
  . (Variant routes → Run (AuthRow messages routes session user eff) (Variant (Namespace Response + responses)))
  → Variant (auth ∷ Route | routes )
  → Run (AuthRow messages routes session user eff) (Variant (Namespace Response + responses))
router = on _auth (localRouter >>> map (inj _auth))

localRouter
  ∷ ∀ eff messages routes session user
  . Route
  → Run (AuthRow messages routes session user eff) Response
localRouter = case _ of
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
      Tuple formLayout Nothing → do
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
logout = method >>= case _ of
  HTTPure.Post → do
    void $ Session.delete
    pure $ LogoutResponse
  method → methodNotAllowed'

withUserRequired ∷ ∀ a eff messages routes session user
  . (User user → Run (AuthRow messages routes session user + eff) a)
  → Run (AuthRow messages routes session user + eff) a
withUserRequired f = Session.fetch >>= _.user >>> case _ of
  Just user → f user
  Nothing → do
    relativeUrl ← Routing.printRoute (namespace Routes.Login)
    redirect (fromRelativeUrl relativeUrl)



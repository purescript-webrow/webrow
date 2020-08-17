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
import WebRow.Applets.Auth.Effects (Auth, User) as Exports
import WebRow.Applets.Auth.Forms (AuthPayload, loginForm)
import WebRow.Applets.Auth.Messages (Messages) as Exports
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..), ResponseRow)
import WebRow.Applets.Auth.Responses (LoginResponse(..), Response(..), ResponseRow) as Exports
import WebRow.Applets.Auth.Routes (Route(..)) as Routes
import WebRow.Applets.Auth.Routes (Route, RouteRow)
import WebRow.Applets.Auth.Routes (localDuplex, routeBuilder, Route(..), RouteRow) as Exports
import WebRow.Applets.Auth.Types (_auth, namespace)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.HTTP (methodNotAllowed', method, redirect)
import WebRow.Routing (fromRelativeUrl)
import WebRow.Routing (printRoute) as Routing
import WebRow.Session (delete, fetch, modify) as Session
import WebRow.Types (WebRow)

type AuthRow messages routes session user eff =
  ( WebRow
    ( authFailed ∷ AuthPayload
    , invalidEmailFormat ∷ String
    , missingValue ∷ Unit
    | messages
    )
    { user ∷ Maybe (User user) | session }
    (RouteRow + routes)
  + Auth user
  + eff
  )

router
  :: ∀ eff messages responses routes routes' session user
  . ( Variant routes
    → Run
      (AuthRow messages routes' session user + eff)
      (Variant (ResponseRow + responses))
    )
  → Variant (RouteRow + routes)
  → Run
    (AuthRow messages routes' session user + eff)
    (Variant (ResponseRow + responses))
router = on _auth (map (inj _auth) <$> localRouter)

localRouter
  ∷ ∀ eff messages routes session user
  . Route
  → Run (AuthRow messages routes session user eff) Response
localRouter = case _ of
  Routes.Login → login
  Routes.Logout → logout

login
  ∷ ∀ eff messages routes session user
  . Run (AuthRow messages routes session user + eff) Response
login = method >>= case _ of
  HTTPure.Post → do
    body ← fromBody
    Forms.Uni.validate loginForm body >>= case _ of
      Tuple Nothing formLayout → do
        pure $ LoginResponse (LoginFormValidationFailed formLayout)
      Tuple (Just user) formLayout → do
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
  void $ Session.delete
  pure $ LogoutResponse

withUserRequired ∷ ∀ a eff messages routes session user
  . (User user → Run (AuthRow messages routes session user + eff) a)
  → Run (AuthRow messages routes session user + eff) a
withUserRequired f = Session.fetch >>= _.user >>> case _ of
  Just user → f user
  Nothing → do
    relativeUrl ← Routing.printRoute (namespace Routes.Login)
    redirect (fromRelativeUrl relativeUrl)



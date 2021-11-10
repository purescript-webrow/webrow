module WebRow.Applets.Auth
  ( module Exports
  , localRouter
  , router
  , withUserRequired
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on)
import HTTPure as HTTPure
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (Auth, User) as Exports
import WebRow.Applets.Auth.Effects (Auth, User, AUTH)
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
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.HTTP (methodNotAllowed', method, redirect)
import WebRow.Routing (fromRelativeUrl)
import WebRow.Routing (printRoute) as Routing
import WebRow.Session (delete, fetch, modify) as Session
import WebRow.Session.SessionStore (TTL)
import WebRow.Types (WebRow)

type AuthRow messages routes session user eff
  = ( WebRow
        ( MissingValue
        + InvalidEmailFormat
        + ( authFailed ∷ AuthPayload
          | messages
          )
        )
        { user ∷ Maybe (User user) | session }
        (RouteRow + routes)
        + AUTH user
        + eff
    )

router ::
  ∀ eff messages responses routes routes' session user.
  TTL →
  ( Variant routes →
    Run
      (AuthRow messages routes' session user + eff)
      (Variant (ResponseRow + responses))
  ) →
  Variant (RouteRow + routes) →
  Run
    (AuthRow messages routes' session user + eff)
    (Variant (ResponseRow + responses))
router ttl = on _auth (map (inj _auth) <$> localRouter ttl)

localRouter ∷
  ∀ eff messages routes session user.
  TTL →
  Route →
  Run (AuthRow messages routes session user eff) Response
localRouter ttl = case _ of
  Routes.Login → login ttl
  Routes.Logout → logout

login ∷
  ∀ eff messages routes session user.
  TTL →
  Run (AuthRow messages routes session user + eff) Response
login ttl =
  method
    >>= case _ of
        HTTPure.Post → do
          body ← fromBody
          Forms.Uni.validate loginForm body
            >>= case _ of
                Tuple Nothing formLayout → do
                  pure $ LoginResponse (LoginFormValidationFailed formLayout)
                Tuple (Just user) formLayout → do
                  Session.modify ttl _ { user = Just user }
                  pure $ LoginResponse LoginSuccess
        HTTPure.Get → do
          let
            form = Forms.Uni.default loginForm
          pure $ LoginResponse (InitialEmailPassordForm form)
        method → methodNotAllowed'

logout ∷
  ∀ eff messages routes session user.
  Run
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

withUserRequired ∷
  ∀ a eff messages routes session user.
  TTL →
  (User user → Run (AuthRow messages routes session user + eff) a) →
  Run (AuthRow messages routes session user + eff) a
withUserRequired ttl f =
  Session.fetch (Just ttl) >>= _.user
    >>> case _ of
        Just user → f user
        Nothing → do
          relativeUrl ← Routing.printRoute (namespace Routes.Login)
          redirect (fromRelativeUrl relativeUrl)

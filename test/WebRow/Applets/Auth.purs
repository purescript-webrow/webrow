module Test.WebRow.Applets.Auth where

import Prelude

import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..))
import Data.Variant (Variant, case_, inj, on)
import Effect.Class (liftEffect) as Effect
import Effect.Class.Console (log)
import Effect.Ref (new, read) as Ref
import JS.Unsafe.Stringify (unsafeStringify)
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, liftEffect, runBaseAff')
import Run (on, run, send) as Run
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Row (type (+))
import WebRow.Applets.Auth (RouteRow, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (AUTH, Auth(..))
import WebRow.Applets.Auth.Routes (Route(..)) as Auth.Routes
import WebRow.Applets.Auth.Testing.Templates (render) as Templates
import WebRow.Applets.Auth.Types (Password(..), _auth)
import WebRow.Mailer (Email(..))
import WebRow.Routing (route)
import WebRow.Session (fetch) as Session
import WebRow.Session.SessionStore (TTL(..))
import WebRow.Testing.HTTP (get, post, run) as T.H
import WebRow.Testing.HTTP.Response (Response(..)) as T.H.R
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret

type Route = Variant (Auth.RouteRow + ())

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build Auth.routeBuilder {}

runAuth ∷ ∀ eff. Run (AUTH () + eff) ~> Run eff
runAuth = Run.run (Run.on _auth handler Run.send)
  where
    handler (Authenticate email password next) = do
      if email == Email "user@example.com" && un Password password == "correct"
        then pure $ next (Just { email })
        else pure $ next Nothing

ttl :: TTL
ttl = TTL $ Seconds $ 60.0 * 60.0 * 24.0 * 3.0

server = Testing.Interpret.runMessage $ runAuth $ bind route $ case_
  # Auth.router ttl

render = case_
  # on _auth Templates.render

spec ∷ Spec Unit
spec = do
  describe "Auth" do
    describe "login" do
      it "flow" do
        ref ← Effect.liftEffect $ Ref.new Map.empty
        let
          sessionStorageConfig =
            { default: { user: Nothing }, ref, key: Nothing }
          client = do
            let
              loginUrl = (D.print routeDuplex (inj _auth Auth.Routes.Login))

            response ← T.H.post loginUrl
              { "email": "user@example.com"
              , "password": "wrong"
              }

            -- | TODO:
            -- | Refector this piece out into something like
            -- | `Webrow.Testing.Spec.post` which expects status code etc.
            liftEffect $ case response of
                T.H.R.HTTPResponse r → r.parts.status `shouldEqual` 200
                T.H.R.HTTPException _ _ → fail "Unexpected exception"

            s ← Session.fetch (Just ttl)
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

            response ← T.H.post loginUrl
              { "email": "user@example.com"
              , "password": "correct"
              }
            liftEffect $ log "\nLogin correct response:"
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch (Just ttl)
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s
            void $ T.H.get "2"

            let
              logoutUrl = (D.print routeDuplex (inj _auth Auth.Routes.Logout))

            response ← T.H.post logoutUrl {}

            liftEffect $ log ("\nLogout response (" <> unsafeStringify logoutUrl <> "):")
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch (Just ttl)
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

        httpSession ← runBaseAff' $ T.H.run sessionStorageConfig routeDuplex render server client
        Effect.liftEffect $ log "\nSession store:"
        Effect.liftEffect $ log =<< (Ref.read ref <#> unsafeStringify)
        pure unit

        -- logShow "The whole session:"
        -- logShow $ unsafeStringify httpSession


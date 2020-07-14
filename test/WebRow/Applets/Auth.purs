module Test.WebRow.Applets.Auth where

import Prelude

import Data.Map (fromFoldableWithIndex) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Variant (Variant, case_, inj, on)
import Effect.Class (liftEffect) as Effect
import Effect.Class.Console (log)
import Effect.Ref (new) as Effect.Ref
import Effect.Ref (read) as Ref
import Foreign.Object (fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Batteries.UrlEncoded.Query (Decoded(..))
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, liftEffect, runBaseAff')
import Run (interpret, liftEffect, on, run, send) as Run
import Run.Reader (askAt)
import Test.Spec (Spec, describe, it)
import Type.Row (type (+))
import WebRow.Applets.Auth (RouteRow, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (Auth, AuthF(..))
import WebRow.Applets.Auth.Routes (Route(..)) as Auth.Routes
import WebRow.Applets.Auth.Testing.Templates (render) as Templates
import WebRow.Applets.Auth.Types (Password(..), _auth)
import WebRow.Contrib.Run (EffRow)
import WebRow.Mailer (Email(..))
import WebRow.Routing (_routing, runRouting)
import WebRow.Session (fetch) as Session
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (forRef) as SessionStore.InMemory
import WebRow.Testing.HTTP (get, post, run) as T.H
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret

type Route = Variant (Auth.RouteRow + ())

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build Auth.routeBuilder {}

runAuth ∷ ∀ eff. Run (Auth () + eff) ~> Run eff
runAuth = Run.run (Run.on _auth handler Run.send)
  where
    handler (Authenticate email password next) = do
      if email == Email "user@example.com" && un Password password == "correct"
        then pure $ next (Just { email })
        else pure $ next Nothing

server = Testing.Interpret.runMessage $ runAuth $ do
  routing ← askAt _routing
  case_ # Auth.router $ routing.route

render = case_
  # on _auth Templates.render

spec ∷ Spec Unit
spec = do
  describe "Auth" do
    describe "login" do
      it "flow" do
        -- | TODO: Could it be a helper in WebRow.Testing.Sesssion?
        ref ← Effect.liftEffect $ Effect.Ref.new mempty
        ss ← Effect.liftEffect do
          SessionStore.hoist Run.liftEffect <$>
            SessionStore.InMemory.forRef ref { user: Nothing }
        let
          client = do
            let
              loginUrl = (D.print routeDuplex (inj _auth Auth.Routes.Login))

            -- response ← T.H.get loginUrl
            response ← T.H.post loginUrl
              { "email": "user@example.com"
              , "password": "wrong"
              }

            liftEffect $ log "\nLogin wrong response:"
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

            response ← T.H.post loginUrl
              { "email": "user@example.com"
              , "password": "correct"
              }
            liftEffect $ log "\nLogin correct response:"
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s
            void $ T.H.get "2"

            let
              logoutUrl = (D.print routeDuplex (inj _auth Auth.Routes.Logout))

            response ← T.H.post logoutUrl {}

            liftEffect $ log ("\nLogout response (" <> unsafeStringify logoutUrl <> "):")
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

        httpSession ← runBaseAff' $ T.H.run ss routeDuplex render server client
        Effect.liftEffect $ log "\nSession store:"
        Effect.liftEffect $ log =<< (Ref.read ref <#> unsafeStringify)
        pure unit

        -- logShow "The whole session:"
        -- logShow $ unsafeStringify httpSession


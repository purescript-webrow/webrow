module Test.WebRow.Applets.Auth where

import Prelude

import Data.Either (Either(..))
import Data.Map (fromFoldable, fromFoldableWithIndex) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Variant (Variant, case_, inj, on)
import Effect.Class (liftEffect) as Effect
import Effect.Class.Console (log, logShow)
import Effect.Random (random)
import Effect.Ref (new) as Effect.Ref
import Effect.Ref (read) as Ref
import Foreign.Object (fromHomogeneous)
import Foreign.Object (fromHomogeneous, toUnfoldable) as Object
import Global.Unsafe (unsafeStringify)
import HTTPure.Headers (empty) as HTTPure.Headers
import Polyform.Batteries.UrlEncoded.Query (Decoded(..))
import Record (merge) as Record
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', parse, print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (EFFECT, Run, liftEffect, runBaseAff')
import Run (interpret, liftEffect, on, send) as Run
import Run.Reader (askAt)
import Test.Spec (Spec, describe, it)
import Test.WebRow.Applets.Templates (toHTTPResponse) as Templates
import Type.Row (type (+))
import WebRow.Applets.Auth (RouteRow, localDuplex, localRouter, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (AUTH, AuthF(..), Auth)
import WebRow.Applets.Auth.Routes (Route(..)) as Auth.Routes
import WebRow.Applets.Auth.Types (Password(..), _auth)
import WebRow.Contrib.Run (EffRow)
import WebRow.HTTP.Response (notFound) as HTTP.Response
import WebRow.Mailer (Email(..))
import WebRow.Routing (_routing, runRouting)
import WebRow.Session (fetch) as Session
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (forRef) as SessionStore.InMemory
import WebRow.Testing.HTTP (get)
import WebRow.Testing.HTTP (get, post, run, run') as T.H
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret

type Route = Variant (Auth.RouteRow + ())

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build Auth.routeBuilder {}

runAuth
  ∷ ∀ eff
  . Run
    ( Auth ()
    + EffRow
    + eff
    )
   ~> Run (EffRow + eff)
runAuth = Run.interpret (Run.on _auth handler Run.send)
  where
    handler ∷ AuthF () ~> Run (EffRow + eff)
    handler (Authenticate email password next) = do
      liftEffect $ log $ "Validating blabla" <> unsafeStringify email
      if email == Email "user@example.com" && un Password password == "correct"
        then pure $ next (Just { email })
        else pure $ next Nothing

server req = runRouting "test.example.com" routeDuplex req $ Testing.Interpret.runMessage $ runAuth $ do
  routing ← askAt _routing
  -- | TODO: FIX THIS
  response ← case_
    # Auth.router
    $ routing.route
  (case_ # on _auth Templates.toHTTPResponse) response

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
            response ← T.H.post loginUrl $ Decoded $ Map.fromFoldableWithIndex $ Object.fromHomogeneous $
              { "email": [ "user@example.com" ]
              , "password": [ "wrong" ]
              }

            liftEffect $ log "\nLogin wrong response:"
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

            response ← T.H.post loginUrl $ Decoded $ Map.fromFoldableWithIndex $ Object.fromHomogeneous $
              { "email": [ "user@example.com" ]
              , "password": [ "correct" ]
              }
            liftEffect $ log "\nLogin correct response:"
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s
            void $ T.H.get "2"

            let
              logoutUrl = (D.print routeDuplex (inj _auth Auth.Routes.Logout))

            response ← T.H.post logoutUrl $ Decoded $ mempty

            liftEffect $ log ("\nLogout response (" <> unsafeStringify logoutUrl <> "):")
            liftEffect $ log $ unsafeStringify response

            s ← Session.fetch
            liftEffect $ log "\nSession:"
            liftEffect $ log $ unsafeStringify s

        httpSession ← runBaseAff' $ T.H.run ss server client
        Effect.liftEffect $ log "\nSession store:"
        Effect.liftEffect $ log =<< (Ref.read ref <#> unsafeStringify)
        pure unit
        -- logShow "The whole session:"
        -- logShow $ unsafeStringify httpSession

-- server = do
--   cs ← Crypto.secret
--   c ← Lazy.force <$> Cookies.lookup "test"
--   liftEffect $ logShow c
--   void $ Cookies.set "test" { value: "test", attributes: Cookies.defaultAttributes }
--   r ← liftEffect $ random
--   ok $ show r

module Test.WebRow.Applets where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, case_, inj)
import Effect.Class.Console (logShow)
import Effect.Random (random)
import Global.Unsafe (unsafeStringify)
import HTTPure.Headers (empty) as HTTPure.Headers
import Record (merge) as Record
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', parse, print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (EFFECT, Run, liftEffect)
import Run (interpret, on, send) as Run
import Run.Reader (askAt)
import Test.Spec (Spec, describe, it)
import Test.WebRow.Applets.Templates (toHTTPResponse) as Templates
import Type.Row (type (+))
import WebRow.Applets.Auth (RouteRow, localDuplex, localRouter, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (AUTH, AuthF(..))
import WebRow.Applets.Auth.Routes (Route(..)) as Auth.Routes
import WebRow.Applets.Auth.Types (_auth)
import WebRow.HTTP.Response (notFound) as HTTP.Response
import WebRow.Routing (_routing, runRouting)
import WebRow.Testing.HTTP (get)
import WebRow.Testing.HTTP (get, run') as T.H
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret


type Route = Variant (Auth.RouteRow + ())

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build Auth.routeBuilder {}

runAuth
  ∷ ∀ eff
  . Run
    ( auth ∷ AUTH ()
    | eff
    )
   ~> Run (eff)
runAuth = Run.interpret (Run.on _auth handler Run.send)
  where
    handler
      ∷ ∀ eff
      . AuthF () ~> Run (eff)
    handler (Authenticate email password next) = do
      pure $ next (Just { email })


server req = runRouting "test.example.com" routeDuplex req $ Testing.Interpret.runMessage $ runAuth $ do
  routing ← askAt _routing
  -- | TODO: FIX THIS
  response ← case_
    # Auth.router
    $ routing.route
  Templates.toHTTPResponse response

spec ∷ Spec Unit
spec = do
  describe "WebRow.Applets.Auth" do
    describe "login" do
      it "failes" do
        let
          client = do
            response ← T.H.get (D.print routeDuplex (inj _auth Auth.Routes.Login))
            liftEffect $ log $ unsafeStringify response
            T.H.get "2"
        httpSession ← T.H.run' { user: Nothing } server client
        logShow $ unsafeStringify httpSession

-- server = do
--   cs ← Crypto.secret
--   c ← Lazy.force <$> Cookies.lookup "test"
--   liftEffect $ logShow c
--   void $ Cookies.set "test" { value: "test", attributes: Cookies.defaultAttributes }
--   r ← liftEffect $ random
--   ok $ show r

module Test.WebRow.Applets.Registration where

import Prelude

import Data.Array (fromFoldable) as Array
import Data.List (List(..)) as List
import Data.Map (fromFoldableWithIndex) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Effect.Class (liftEffect) as Effect
import Effect.Class.Console (log, logShow)
import Effect.Ref (new) as Effect.Ref
import Effect.Ref (read) as Ref
import Foreign.Object (fromHomogeneous) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run(..), runBaseAff')
import Run (interpret, liftEffect, on, send) as Run
import Run.Reader (askAt, runReaderAt)
import Run.Writer (runWriterAt)
import Test.Spec (Spec, describe, it)
import Test.WebRow.Applets.Auth (runAuth)
import Test.WebRow.Applets.Registration.Templates (render) as R.Templates
import Test.WebRow.Applets.Templates (toHTTPResponse) as Templates
import Type.Row (type (+))
import WebRow.Applets.Auth (RouteRow, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration (Route(..)) as Registration.Routes
import WebRow.Applets.Registration (RouteRow, router) as Registration
import WebRow.Applets.Registration (routeBuilder) as Registartion
import WebRow.Applets.Registration.Effects (Registration, RegistrationF(..))
import WebRow.Applets.Registration.Types (_registration)
import WebRow.Mailer (_mailer)
import WebRow.Routing (_routing, runRouting)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (forRef) as SessionStore.InMemory
import WebRow.Testing.HTTP (post, run) as T.H
import WebRow.Testing.Interpret (_mailQueue, runMailer)
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret

type Route = Variant (Auth.RouteRow + Registration.RouteRow + ())
-- type Route = Variant (Registration.RouteRow + ())

routeDuplex ∷ D.RouteDuplex' Route
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build (Auth.routeBuilder <<< Registartion.routeBuilder) {}
    -- routes = Record.Builder.build (Registartion.routeBuilder) {}
runRegistration
  ∷ ∀ eff
  . Run
    ( Registration
    + eff
    )
   ~> Run eff
runRegistration = Run.interpret (Run.on _registration handler Run.send)
  where
    handler ∷ RegistrationF ~> Run eff
    handler (EmailTaken email next) = pure (next true)

render = case_
  # on _auth Templates.toHTTPResponse
  # on _registration R.Templates.render

server req =  runRouting "test.example.com" routeDuplex req $ Testing.Interpret.runMessage do
  routing ← askAt _routing
  -- | TODO: FIX THIS
  response ← case_
    # Registration.router
    # Auth.router
    $ routing.route
  render response

spec ∷ Spec Unit
spec = do
  describe "Registration" do
    describe "registerEmail" do
      it "flow" do
        -- | TODO: Could it be a helper in WebRow.Testing.Sesssion?
        ref ← Effect.liftEffect $ Effect.Ref.new mempty
        ss ← Effect.liftEffect do
          SessionStore.hoist Run.liftEffect <$>
            SessionStore.InMemory.forRef ref { user: Nothing }
        let
          client = do
            let
              registrationUrl = (D.print routeDuplex (inj _registration Registration.Routes.RegisterEmail))

            response ← T.H.post registrationUrl $ Decoded $ Map.fromFoldableWithIndex $ Object.fromHomogeneous $
              { "email": [ "user@example.com" ] }
            pure unit

        Tuple mails httpSession ← runBaseAff' $ runAuth $ runWriterAt _mailQueue $ runMailer $ runRegistration $ (T.H.run ss server client)

        log "\nMails:"
        log $ unsafeStringify $ Array.fromFoldable mails

        log "\nThe whole session:"
        logShow $ unsafeStringify httpSession


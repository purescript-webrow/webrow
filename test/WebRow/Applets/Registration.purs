module Test.WebRow.Applets.Registration where

import Prelude

import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Effect.Class (liftEffect) as Effect
import Effect.Ref (new) as Effect.Ref
import Record.Builder (build) as Record.Builder
import Routing.Duplex (RouteDuplex', print, root) as D
import Routing.Duplex.Generic.Variant (variant') as RouteDuplex.Variant
import Run (Run, runBaseAff')
import Run (liftEffect, on, run, send) as Run
import Run.Reader (askAt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.WebRow.Applets.Auth (runAuth)
import Type.Row (type (+))
import WebRow.Applets.Auth (Messages, ResponseRow, RouteRow, routeBuilder, router) as Auth
import WebRow.Applets.Auth.Effects (Auth)
import WebRow.Applets.Auth.Testing.Templates (render) as A.Templates
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration (Messages, ResponseRow, RouteRow, router) as Registration
import WebRow.Applets.Registration (Route(..)) as Registration.Routes
import WebRow.Applets.Registration (routeBuilder) as Registartion
import WebRow.Applets.Registration.Effects (Registration, RegistrationF(..))
import WebRow.Applets.Registration.Testing.Templates (render) as R.Templates
import WebRow.Applets.Registration.Types (_registration)
import WebRow.Crypto (Crypto)
import WebRow.HTTP (HTTPResponse)
import WebRow.Mailer (Email(..), Mailer)
import WebRow.Routing (FullUrl, Routing', _routing)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (forRef) as SessionStore.InMemory
import WebRow.Testing.HTTP (post_, run) as T.H
import WebRow.Testing.Interpret (runMailer')
import WebRow.Testing.Interpret (runMessage) as Testing.Interpret
import WebRow.Types (WebRow)

type RouteRow = (Auth.RouteRow + Registration.RouteRow + ())

type MessageRow = (Auth.Messages + Registration.Messages + ())

type ResponseRow = (Auth.ResponseRow + Registration.ResponseRow + ())

routeDuplex ∷ D.RouteDuplex' (Variant RouteRow)
routeDuplex = D.root $ RouteDuplex.Variant.variant' routes
  where
    routes = Record.Builder.build
      (Auth.routeBuilder <<< Registartion.routeBuilder)
      {}

runRegistration ∷ ∀ eff. Run (Registration + eff) ~> Run eff
runRegistration = Run.run (Run.on _registration handler Run.send)
  where
    handler (EmailTaken (Email email) next) =
      pure (next $ email == "already-registered@example.com")

render
  ∷ ∀ eff
  . Variant ResponseRow
  → Run (Routing' RouteRow + eff) (HTTPResponse String)
render = case_
  # on _auth A.Templates.render
  # on _registration R.Templates.render

type UserSession = { user ∷ Maybe { email ∷ Email }}

-- | Handling through this localRouter
-- x =
--   { registration: Registration.localRouter
--   , auth: Auth.localRouter
--   }

server
  ∷ ∀ eff mails
  . Run
      ( Auth ()
      + Crypto
      + Mailer (emailVerification ∷ FullUrl | mails)
      + Registration
      + WebRow
          MessageRow
          UserSession
          RouteRow
      + eff
      )
      (Variant ResponseRow)
server =  do
  routing ← askAt _routing
  case_
    # Registration.router
    # Auth.router
    $ routing.route

spec ∷ Spec Unit
spec = do
  describe "Registration" do
    describe "registerEmail" do
      let
        run ref c = do
          ss ← Effect.liftEffect do
            SessionStore.hoist Run.liftEffect <$>
              SessionStore.InMemory.forRef ref { user: Nothing }
          runBaseAff'
            $ Testing.Interpret.runMessage
            $ runAuth
            $ runMailer'
            $ runRegistration
            $ (T.H.run ss routeDuplex render server c)

      it "fails for already registered email" do
        ref ← Effect.liftEffect $ Effect.Ref.new mempty

        Tuple mails httpSession ← run ref do
          let
            registrationUrl = (D.print routeDuplex (inj _registration Registration.Routes.RegisterEmail))
          T.H.post_ registrationUrl { "email": "already-registered@example.com" }

        length mails `shouldEqual` 0

      it "sends registration mail to new address" do
        ref ← Effect.liftEffect $ Effect.Ref.new mempty

        Tuple mails httpSession ← run ref do
            let
              registrationUrl = (D.print routeDuplex (inj _registration Registration.Routes.RegisterEmail))
            T.H.post_ registrationUrl { "email": "not-taken@example.com" }

        length mails `shouldEqual` 1


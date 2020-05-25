module Main where

import Prelude

import Data.Either (either)
import Data.Variant (Variant, case_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import Record (merge) as Record
import Routing.Duplex as D
import Routing.Duplex.Generic.Variant (variant')
import Run (AFF, EFFECT, Run, runBaseAff')
import Run as Run
import Run.Reader (READER, runReader)
import Type.Row (type (+))
import WebRow.Applets.Auth as Auth
import WebRow.Applets.Auth.Routes as Auth.Routes
import WebRow.Applets.Registration (router) as Register
import WebRow.Applets.Registration.Routes (duplexes, RouteRow) as Registration.Routes
import WebRow.Applets.Registration.Templates.Dummy (onRegister) as Registration.Templates.Dummy
import WebRow.Crypto (Secret(..))
import WebRow.Logging.Effect (LOGGER, runLoggerConsole)
import WebRow.Mailer (MailerF(..), MAILER, _mailer)
import WebRow.Registration.Interpret.Dummy (interpret) as Registration.Interpret.Dummy
import WebRow.Response (onHttpError)
import WebRow.Response (runResponse) as Response
import WebRow.Route (interpret) as Route

type Routes = Variant
  ( Registration.Routes.RouteRow
  -- + Auth.Routes.RouteRow
  + ())

route ∷ D.RouteDuplex' Routes
route = D.root $ variant'
  $ {}
  `Record.merge` Registration.Routes.duplexes
  -- `Record.merge` Auth.Routes.duplexes

type BaseEffects =
  ( aff ∷ AFF
  , effect ∷ EFFECT
  , logger ∷ LOGGER
  , mailer ∷ MAILER
  , reader ∷ READER { request ∷ HTTPure.Request, secret ∷ Secret }
  )

interpretMailerStub
  ∷ ∀ a eff
  . Run ( aff ∷ AFF , mailer ∷ MAILER | eff ) a
  → Run ( aff ∷ AFF                   | eff ) a
interpretMailerStub = Run.interpret (Run.on _mailer handleMailerStub Run.send)

handleMailerStub ∷ ∀ a eff. MailerF a → Run ( aff ∷ AFF | eff ) a
handleMailerStub (SendMail r k) = do
  Run.liftAff $ log $ "Mail sent: " <> show r
  pure $ k "mail sent - stub no data"

interpretBaseEffects ∷ HTTPure.Request → Run BaseEffects ~> Aff
interpretBaseEffects request
  = runBaseAff'
  <<< runLoggerConsole
  <<< runReader { request, secret: Secret "g6f7s8r328h0ej906291d" }
  <<< interpretMailerStub

-- interpret ∷ HTTPure.Request → Routes → Aff HTTPure.Response
runApps ∷ Routes → Run BaseEffects HTTPure.Response
runApps
  = httpResponse
  <<< interpretApps
  <<< Route.interpret { domain: "http://localhost:8080", route }
  <<< router
  where
    router = case_
      # Register.router
      -- # Auth.router

    -- | Run app effects
    interpretApps
      = Registration.Interpret.Dummy.interpret
      -- >>> Auth.Interpret.interpret

    -- | Collect responses from row and
    -- | turn them into HTTPure responses.
    -- |
    -- | Because HTTPure provides helpers which
    -- | build responses in the monad we have
    -- | to use kleisly composition here.
    httpResponse = Response.runResponse >=> toHTTPResponse
      where
        toHTTPResponse = case_
          # onHttpError
          # Registration.Templates.Dummy.onRegister

app ∷ HTTPure.Request → Aff HTTPure.Response
app req = do
  let
    url = HTTPure.fullPath req
  D.parse route url # either (HTTPure.badRequest <<< show) (interpretBaseEffects req <<< runApps)


main ∷ Effect Unit
main = do
  void $ HTTPure.serve 8080 app $ log "Server now up on port 8080"

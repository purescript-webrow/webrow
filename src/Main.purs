module Main where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, case_)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Class.Console (log)
import Effect.Ref as Ref
import HTTPure as HTTPure
import Record (merge) as Record
import Routing.Duplex as D
import Routing.Duplex.Generic.Variant (variant')
import Run (AFF, EFFECT, Run, runBaseAff')
import Run as Run
import Run.Except (EXCEPT, runExcept)
import Run.Reader (runReader)
import SeldaUtils.App (setupDB)
import SeldaUtils.Effect (SELDA, interpretSeldaPG)
import Type.Row (type (+))
import WebRow.Applets.Auth as Auth
import WebRow.Applets.Auth.Routes as Auth.Routes
import WebRow.Applets.Auth.Templates.Dummy as Auth.Templates.Dummy
import WebRow.Applets.Registration (router) as Register
import WebRow.Applets.Registration.Routes (duplexes, RouteRow) as Registration.Routes
import WebRow.Applets.Registration.Templates.Dummy as Registration.Templates.Dummy
import WebRow.Auth.Interpret (createUsersTable)
import WebRow.Auth.Interpret as Auth.Interpret
import WebRow.Crypto (Secret(..))
import WebRow.Logging.Effect (LOGGER, runLoggerConsole)
import WebRow.Logging.Effect as Log
import WebRow.Mailer (Email, MAILER, MailerF(..), _mailer)
import WebRow.Reader as WebRow.Reader
import WebRow.Registration.Interpret.Dummy (interpret) as Registration.Interpret.Dummy
import WebRow.Response (RESPONSE, internalServerError, runResponseBase)
import WebRow.Response.Modify (MODIFY)
import WebRow.Response.Modify as Response.Modify
import WebRow.Route (interpret) as Route
import WebRow.Session (SESSIONSTORE)
import WebRow.Session.DataStore (memoryStore, runSessionStoreInMemory)
import WebRow.Session.Effect (SESSION, withSession)

type Routes = Variant
  ( Registration.Routes.RouteRow
  + Auth.Routes.RouteRow
  + ())

route ∷ D.RouteDuplex' Routes
route = D.root $ variant'
  $ {}
  `Record.merge` Registration.Routes.duplexes
  `Record.merge` Auth.Routes.duplexes

interpretMailerStub
  ∷ ∀ a eff
  . Run ( aff ∷ AFF , mailer ∷ MAILER | eff ) a
  → Run ( aff ∷ AFF                   | eff ) a
interpretMailerStub = Run.interpret (Run.on _mailer handleMailerStub Run.send)

handleMailerStub ∷ ∀ a eff. MailerF a → Run ( aff ∷ AFF | eff ) a
handleMailerStub (SendMail r k) = do
  Run.liftAff $ log $ "Mail sent: " <> show r
  pure $ k "mail sent - stub no data"

interpretBaseEffects
  ∷ HTTPure.Request
  → Run
      ( aff ∷ AFF
      , effect ∷ EFFECT
      , modifyResponse ∷ MODIFY
      , response ∷ RESPONSE ()
      , logger ∷ LOGGER
      , reader ∷ WebRow.Reader.READER ( dbConn ∷ PG.Connection )
      , except ∷ EXCEPT (Variant ( selda ∷ PG.PGError ))
      , selda ∷ SELDA
      , store ∷ SESSIONSTORE { user ∷ Maybe Email }
      , session ∷ SESSION { user ∷ Maybe Email }
      )
      HTTPure.Response
  → Aff HTTPure.Response
interpretBaseEffects request m = do
  ref ← Class.liftEffect $ Ref.new mempty
  setupDB \dbConn → m 
    # runBaseAff'
      -- response handling
      <<< Response.Modify.run
      <<< runResponseBase

      <<< runLoggerConsole

      -- selda
      <<< runExcept500
      <<< runReader { request, secret: Secret "g6f7s8r328h0ej906291d", dbConn }
      <<< interpretSeldaPG
      <<< (createUsersTable *> _)

      -- session
      <<< runSessionStoreInMemory (memoryStore ref)
      <<< withSession (pure { user: Nothing })

runExcept500
  ∷ ∀ e a res eff
  . Show e
  ⇒ Run ( except ∷ EXCEPT e, response ∷ RESPONSE res, logger ∷ LOGGER | eff ) a
  → Run (                    response ∷ RESPONSE res, logger ∷ LOGGER | eff ) a
runExcept500 = runExcept >=> flip either pure \e → do
  Log.err $ show e
  internalServerError HTTPure.empty ""

app ∷ HTTPure.Request → Aff HTTPure.Response
app req = do
  let
    url = HTTPure.fullPath req
  D.parse route url # either
    (HTTPure.badRequest <<< show)
    affRouter
  where
    affRouter ∷ Routes → Aff HTTPure.Response
    affRouter
      = interpretBaseEffects req
      <<< Route.interpret { domain: "http://localhost:8080", route }
      <<< ( Auth.Interpret.interpret
          >>> Auth.Templates.Dummy.runResponseAuth 
          )
      <<< ( Registration.Interpret.Dummy.interpret
          >>> Registration.Templates.Dummy.runResponseRegister
          >>> interpretMailerStub
          )
      <<< router

    router = case_
      # Auth.router
      # Register.router

main ∷ Effect Unit
main = do
  void $ HTTPure.serve 8080 app $ log "Server now up on port 8080"

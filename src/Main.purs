module Main where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, case_, on)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Class.Console (log)
import Effect.Ref as Ref
import HTTPure as HTTPure
import Record (merge) as Record
import Routing.Duplex as D
import Routing.Duplex.Generic.Variant (variant')
import Run (AFF, EFFECT, Run, liftEffect, runBaseAff')
import Run as Run
import Run.Except (runExcept)
import Run.Reader (READER, runReader)
import SeldaUtils.App (setupDB)
import SeldaUtils.Effect (SELDA, interpretPG)
import Type.Row (type (+))
import WebRow.Applets.Auth as Auth
import WebRow.Applets.Auth.Effects (AUTH)
import WebRow.Applets.Auth.Routes as Auth.Routes
import WebRow.Applets.Auth.Templates.Dummy as Auth.Templates.Dummy
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration (router) as Register
import WebRow.Applets.Registration.Effects (REGISTER)
import WebRow.Applets.Registration.Routes (duplexes, RouteRow) as Registration.Routes
import WebRow.Applets.Registration.Templates.Dummy (onRegister) as Registration.Templates.Dummy
import WebRow.Auth.Interpret as Auth.Interpret
import WebRow.Crypto (Secret(..))
import WebRow.Logging.Effect (LOGGER, runLoggerConsole)
import WebRow.Mailer (MailerF(..), MAILER, _mailer)
import WebRow.Registration.Interpret.Dummy (interpret) as Registration.Interpret.Dummy
import WebRow.Response (RESPONSE, Response, onHttpError)
import WebRow.Response (runResponse) as Response
import WebRow.Response.Modify as Response.Modify
import WebRow.Route (ROUTE)
import WebRow.Route (interpret) as Route
import WebRow.Session.DataStore (memoryStore, runSessionStoreInMemory)
import WebRow.Session.Effect (withSession)

type Routes = Variant
  ( Registration.Routes.RouteRow
  + Auth.Routes.RouteRow
  + ())

route ∷ D.RouteDuplex' Routes
route = D.root $ variant'
  $ {}
  `Record.merge` Registration.Routes.duplexes
  `Record.merge` Auth.Routes.duplexes

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

-- interpretBaseEffects ∷ HTTPure.Request → Run BaseEffects ~> Aff
interpretBaseEffects request m = do
  ref ← Class.liftEffect $ Ref.new mempty
  setupDB \conn → go conn ref m
  where
    go dbConn ref = runBaseAff'
      <<< runExcept
      <<< Response.Modify.run
      <<< runLoggerConsole
      <<< runReader { request, secret: Secret "g6f7s8r328h0ej906291d", dbConn }
      <<< interpretMailerStub
      <<< interpretPG
      <<< runSessionStoreInMemory (memoryStore ref)
      <<< withSession (pure { user: Nothing })

-- runApps ∷ Routes → Run _ HTTPure.Response
runApps
  = httpResponse
  <<< interpretApps
  <<< Route.interpret { domain: "http://localhost:8080", route }
  <<< router

-- | Collect responses from row and
-- | turn them into HTTPure responses.
-- |
-- | Because HTTPure provides helpers which
-- | build responses in the monad we have
-- | to use kleisly composition here.
-- httpResponse :: forall t87.
--   Run
--     ( aff :: AFF
--     , effect :: EFFECT
--     , response :: RESPONSE ( auth :: _, register :: _ )
--     | t87
--     )
--     (Response
--        ( auth :: _
--        , register :: _
--        )
--     )
--   -> Run
--        ( aff :: AFF
--        , effect :: EFFECT
--        | t87
--        )
--        _
httpResponse = Response.runResponse >=> toHTTPResponse
  where
    toHTTPResponse = case_
      # onHttpError
      # Auth.Templates.Dummy.onAuth
      # Registration.Templates.Dummy.onRegister

-- router
--   ∷ ∀ t16 ctx
--   . Variant
--     ( auth ∷ _
--     , register ∷ _
--     )
--   -> Run
--     ( aff :: AFF
--     , auth :: AUTH
--     , logger :: LOGGER
--     , mailer :: MAILER
--     , reader :: FProxy
--                   (Reader
--                     { request :: { body :: String
--                                   , headers :: Headers
--                                   , httpVersion :: Version
--                                   , method :: Method
--                                   , path :: Array String
--                                   , query :: Object String
--                                   , url :: String
--                                   }
--                     , secret :: Secret
--                     | ctx
--                     }
--                   )
--     , register :: FProxy RegisterF
--     , response :: FProxy
--                     (ResponseF
--                       ( auth :: Response
--                       , register :: Response
--                       | t19
--                       )
--                     )
--     , route :: FProxy
--                 (RouteF
--                     ( auth :: Route
--                     , register :: Route
--                     )
--                 )
--     , session :: FProxy
--                   (SessionF
--                       { user :: Maybe Email
--                       | t10
--                       }
--                   )
--     | t20
--     )
--     t16
router = case_
  # Auth.router
  # Register.router

-- | Run app effects
-- interpretApps
--   ∷ ∀ a eff
--   . Run
--       ( effect ∷ EFFECT
--       , selda ∷ SELDA
--       , auth ∷ AUTH
--       , register ∷ REGISTER
--       | eff
--       )
--       a
--   → Run
--       ( effect ∷ EFFECT
--       , selda ∷ SELDA
--       | eff
--       )
--       a
interpretApps
  = Auth.Interpret.interpret
  >>> Registration.Interpret.Dummy.interpret

-- app ∷ HTTPure.Request → Aff HTTPure.Response
-- app req = do
--   let
--     url = HTTPure.fullPath req
--   D.parse route url # either
--     (HTTPure.badRequest <<< show)
--     (interpretBaseEffects req <<< runApps)

aux req = interpretBaseEffects req <<< runApps

-- main ∷ Effect Unit
-- main = do
--   void $ HTTPure.serve 8080 app $ log "Server now up on port 8080"

module Main where

import Prelude

import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (un)
import Data.Variant (case_, on)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Run (AFF, Run, runBaseAff)
import Run as Run
import Run.Reader (READER, runReader)
import ShopUtils.Crypto (Secret(..))
import ShopUtils.Logging.Effect (LOGGER, runLoggerConsole)
import ShopUtils.Mailer (MailerF(..), MAILER, _mailer)
import ShopUtils.Register (_register, handleRegisterResponse)
import ShopUtils.Register as Register
import ShopUtils.Response (Response(..), runResponse)

data Route
  = RegisterRoute Register.Route
  -- | PaymentRoute Payment.Route
derive instance genericRoute ∷ Generic Route _

route ∷ D.RouteDuplex' Route
route = D.root $ DG.sum
  { "RegisterRoute": Register.route
  -- , "PaymentRoute": Payment.route
  }

router ∷ HTTPure.Request → Aff HTTPure.Response
router req = do
  let url = HTTPure.fullPath req
  -- log url
  D.parse route url # either (HTTPure.badRequest <<< show)
    (interpretBaseEffects <<< onRootRoute)

onRootRoute ∷ Route → BaseRun HTTPure.Response
onRootRoute = (handleResponses <<< un Response) <=< runResponse <<< case _ of
  RegisterRoute rr → Register.onRegisterRoute rr # Register.runRegister
  where
    handleResponses = case_
      # on _register handleRegisterResponse

main ∷ Effect Unit
main = do
  -- log $ D.print Register.route $ Register.RegisterEmail $ Email "someemail@gmail.com"
  void $ HTTPure.serve 8080 router $ log "Server now up on port 8080"

type BaseRun = Run
  ( aff ∷ AFF
  , logger ∷ LOGGER
  , mailer ∷ MAILER
  , reader ∷ READER { secret ∷ Secret }
  )

interpretBaseEffects ∷ BaseRun ~> Aff
interpretBaseEffects = interpretMailerStub
  >>> runReader { secret: Secret "g6f7s8r328h0ej906291d" }
  >>> runLoggerConsole
  >>> runBaseAff

interpretMailerStub
  ∷ ∀ a eff
  . Run ( aff ∷ AFF , mailer ∷ MAILER | eff ) a
  → Run ( aff ∷ AFF                   | eff ) a
interpretMailerStub = Run.interpret (Run.on _mailer handleMailerStub Run.send)

handleMailerStub ∷ ∀ a eff. MailerF a → Run ( aff ∷ AFF | eff ) a
handleMailerStub (SendMail r k) = do
  Run.liftAff $ log $ "Mail sent: " <> show r
  pure $ k "mail sent - stub no data"

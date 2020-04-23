module Main where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import Routing.Duplex as D
import Run (AFF, Run, runBaseAff)
import Run as Run
import Run.Reader (runReader)
import ShopUtils.Crypto (Secret(..))
import ShopUtils.Logging.Effect (runLoggerConsole)
import ShopUtils.Mailer (MailerF(..), MAILER, _mailer)
import ShopUtils.Register as Register

router ∷ HTTPure.Request → Aff HTTPure.Response
router req = do
  let url = HTTPure.fullPath req
  -- log url
  D.parse (D.root Register.route) url
    # either (HTTPure.badRequest <<< show) \route → do
      Register.onRegisterRoute route
        # Register.runRegister
        # interpretMailerStub
        # runReader { secret: Secret "g6f7s8r328h0ej906291d" }
        # runLoggerConsole
        # runBaseAff

main ∷ Effect Unit
main = do
  -- log $ D.print Register.route $ Register.RegisterEmail $ Email "someemail@gmail.com"
  void $ HTTPure.serve 8080 router $ log "Server now up on port 8080"

interpretMailerStub
  ∷ ∀ a eff
  . Run ( aff ∷ AFF , mailer ∷ MAILER | eff ) a
  → Run ( aff ∷ AFF                   | eff ) a
interpretMailerStub = Run.interpret (Run.on _mailer handleMailerStub Run.send)

handleMailerStub ∷ ∀ a eff. MailerF a → Run ( aff ∷ AFF | eff ) a
handleMailerStub (SendMail r k) = do
  Run.liftAff $ log $ "Mail sent: " <> show r
  pure $ k "mail sent - stub no data"

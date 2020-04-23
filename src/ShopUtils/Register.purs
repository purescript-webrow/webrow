module ShopUtils.Register where

import Prelude

import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import HTTPure as HTTPure
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Run (AFF, FProxy, Run, SProxy(..))
import Run as Run
import Run.Reader (READER)
import ShopUtils.Crypto (Secret, sign, unsign)
import ShopUtils.Logging.Effect (LOGGER)
import ShopUtils.Logging.Effect as LogEff
import ShopUtils.Mailer (MAILER, sendMail)
import ShopUtils.Types (Email(..), Password, SignedEmail(..))

data Route
  = RegisterEmail Email
  | RegisterPassword { email ∷ SignedEmail, password ∷ Maybe Password }
derive instance genericRoute ∷ Generic Route _

route ∷ D.RouteDuplex' Route
route = D.path "register" $ DG.sum
  { "RegisterEmail": (_Newtype $ D.param "email" ∷ D.RouteDuplex' Email)
  , "RegisterPassword": D.path "form" $ D.params
      { email: _Newtype <<< D.string
      , password: D.optional <<< _Newtype <<< D.string
      }
  }

checkIfTaken ∷ ∀ eff. Email → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) Unit
checkIfTaken email = do
  LogEff.warning $ "checkIfTaken " <> show email

onRegisterRoute
  ∷ ∀ eff ctx a
  . Route
  → Run
      ( aff ∷ AFF
      , logger ∷ LOGGER
      , mailer ∷ MAILER
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      | eff
      )
      a
onRegisterRoute = case _ of
  RegisterEmail email → registerEmail email
  RegisterPassword { email, password } → registerPassword email password

registerEmail
  ∷ ∀ eff ctx a
  . Email
  → Run
      ( aff ∷ AFF
      , logger ∷ LOGGER
      , mailer ∷ MAILER
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      | eff
      )
      a
registerEmail email = do
  -- validateEmail email
  checkIfTaken email -- either actual db function with concrete SQL table or a function of RegisterEffect
  signedEmail ← sign $ un Email email
  text ← printRegisterRoute $ RegisterPassword { email: SignedEmail signedEmail, password: Nothing }
  _ ← sendMail { to: email, text, subject: "Email verification" }
  response $ EmailSent email

registerPassword
  ∷ ∀ eff ctx a
  . SignedEmail
  → Maybe Password
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      | eff
      )
      a
registerPassword signedEmail maybePassword = do
  email ← unsign (un SignedEmail signedEmail) >>= either onInvalidSig pure 
  case maybePassword of
    Nothing → response RenderPasswordForm
    Just password → do
      -- validatePassword password
      response $ CreateAccount (Email email) password
  where
    onInvalidSig err = do
      -- TODO: log err
      response InvalidEmailSignature

runRegister
  ∷ ∀ eff
  . Run ( aff ∷ AFF, logger ∷ LOGGER, register ∷ REGISTER | eff ) HTTPure.Response
  → Run ( aff ∷ AFF, logger ∷ LOGGER                      | eff ) HTTPure.Response
runRegister = Run.run (Run.on _register handleRegister Run.send)

handleRegister
  ∷ ∀ m eff
  . Monad m
  ⇒ RegisterF
      (Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) HTTPure.Response)
  → m (Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) HTTPure.Response)
handleRegister = case _ of
  PrintRegisterRoute r k → do
    pure $ k $ D.print route r
  AssertEmailNotTaken email next → do
    pure next
  Response rr → pure case rr of
    InvalidEmailSignature →
      Run.liftAff $ HTTPure.badRequest "InvalidEmailSignature"
    RenderPasswordForm → 
      ok "Password From (please provide a password)"
    CreateAccount email password → do
      -- TODO: register in the db and handle the result further
      LogEff.err $ "Effect (CreateAccount " <> show email <> " " <> show password <> ") not handled"
      Run.liftAff $ HTTPure.internalServerError "Registration failed"
    EmailSent email →
      ok $ "Email sent to " <> show email
  where
    ok = Run.liftAff <<< HTTPure.ok

data RegisterF a
  = PrintRegisterRoute Route (String → a) -- ^ 
  | AssertEmailNotTaken Email a
  | Response RegisterResponse

derive instance functorRegisterF ∷ Functor RegisterF

data RegisterResponse
  = InvalidEmailSignature
  | RenderPasswordForm
  | CreateAccount Email Password
  | EmailSent Email

type REGISTER = FProxy RegisterF

_register = SProxy ∷ SProxy "register"

printRegisterRoute ∷
  ∀ eff
  . Route
  → Run ( register ∷ REGISTER | eff ) String
printRegisterRoute rr = Run.lift _register (PrintRegisterRoute rr identity)

assertEmailNotTaken ∷
  ∀ eff
  . Email
  → Run ( register ∷ REGISTER | eff ) Unit
assertEmailNotTaken email = Run.lift _register (AssertEmailNotTaken email unit)

response
  ∷ ∀ a eff
  . RegisterResponse
  → Run ( register ∷ REGISTER | eff ) a
response x = Run.lift _register (Response x)

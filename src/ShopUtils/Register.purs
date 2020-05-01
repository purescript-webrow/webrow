module ShopUtils.Register where

import Prelude

import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Variant (inj)
import HTTPure as HTTPure
import Prim.Row as Row
import Record as Record
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Run (AFF, FProxy, Run, SProxy(..))
import Run as Run
import Run.Reader (READER)
import ShopUtils.Crypto (Secret, sign, unsign)
import ShopUtils.Logging.Effect (LOGGER)
import ShopUtils.Logging.Effect as LogEff
import ShopUtils.Mailer (MAILER, sendMail)
import ShopUtils.Response (RESPONSE, response)
import ShopUtils.Route (ROUTE, printFullRoute)
import ShopUtils.Types (Email(..), Password, SignedEmail(..))

insertRoute
  ∷ ∀ rs
  . Row.Lacks "register" rs
  ⇒ { | rs }
  → { register ∷ D.RouteDuplex' Route | rs }
insertRoute = Record.insert _register route

data Route
  = RegisterEmail Email
  | RegisterPassword { email ∷ SignedEmail, password ∷ Maybe Password }
derive instance genericRoute ∷ Generic Route _

route ∷ D.RouteDuplex' Route
route = DG.sum
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
  ∷ ∀ eff ctx res rs a
  . Route
  → Run
      ( aff ∷ AFF
      , logger ∷ LOGGER
      , mailer ∷ MAILER
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      , response ∷ RESPONSE ( register ∷ RegisterResponse | res )
      , route ∷ ROUTE ( register ∷ Route | rs )
      | eff
      )
      a
onRegisterRoute = case _ of
  RegisterEmail email → registerEmail email
  RegisterPassword { email, password } → registerPassword email password

registerEmail
  ∷ ∀ eff ctx res rs a
  . Email
  → Run
      ( aff ∷ AFF
      , logger ∷ LOGGER
      , mailer ∷ MAILER
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      , response ∷ RESPONSE ( register ∷ RegisterResponse | res )
      , route ∷ ROUTE ( register ∷ Route | rs )
      | eff
      )
      a
registerEmail email = do
  -- validateEmail email
  checkIfTaken email -- either actual db function with concrete SQL table or a function of RegisterEffect
  signedEmail ← sign $ un Email email
  text ← printFullRoute $ inj _register $ RegisterPassword { email: SignedEmail signedEmail, password: Nothing }
  _ ← sendMail { to: email, text, subject: "Email verification" }
  response $ inj _register $ EmailSent email

registerPassword
  ∷ ∀ eff ctx res a
  . SignedEmail
  → Maybe Password
  → Run
      ( aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      , register ∷ REGISTER
      , response ∷ RESPONSE ( register ∷ RegisterResponse | res )
      | eff
      )
      a
registerPassword signedEmail maybePassword = do
  email ← unsign (un SignedEmail signedEmail) >>= either onInvalidSig pure 
  case maybePassword of
    Nothing → response $ inj _register $ RenderPasswordForm
    Just password → do
      -- validatePassword password
      response $ inj _register $ CreateAccount (Email email) password
  where
    onInvalidSig err = do
      -- TODO: log err
      response $ inj _register InvalidEmailSignature

runRegister
  ∷ ∀ eff a
  . Run ( aff ∷ AFF, logger ∷ LOGGER, register ∷ REGISTER | eff ) a
  → Run ( aff ∷ AFF, logger ∷ LOGGER                      | eff ) a
runRegister = Run.interpret (Run.on _register handleRegister Run.send)

handleRegister
  ∷ ∀ eff
  . RegisterF ~> Run ( aff ∷ AFF, logger ∷ LOGGER | eff )
handleRegister = case _ of
  AssertEmailNotTaken email next → do
    pure next

handleRegisterResponse
  ∷ ∀ eff 
  . RegisterResponse
  → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) HTTPure.Response
handleRegisterResponse = case _ of
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
  where ok = Run.liftAff <<< HTTPure.ok

data RegisterF a
  = AssertEmailNotTaken Email a

derive instance functorRegisterF ∷ Functor RegisterF

data RegisterResponse
  = InvalidEmailSignature
  | RenderPasswordForm
  | CreateAccount Email Password
  | EmailSent Email

type REGISTER = FProxy RegisterF

_register = SProxy ∷ SProxy "register"

assertEmailNotTaken ∷
  ∀ eff
  . Email
  → Run ( register ∷ REGISTER | eff ) Unit
assertEmailNotTaken email = Run.lift _register (AssertEmailNotTaken email unit)

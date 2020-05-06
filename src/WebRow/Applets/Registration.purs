module WebRow.Applets.Registration where

import Prelude

import Control.Monad.Reader.Class (asks)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant, inj, on)
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..), Request) as HTTPure
import Node.FS (FileFlags(..))
import Polyform.Validator (runValidator)
import Routing.Duplex (RouteDuplex', (:=))
import Routing.Duplex as D
import Routing.Duplex.Generic as DG
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteParams)
import Run (AFF, FProxy, Run, SProxy(..))
import Run as Run
import Run.Reader (READER, ask)
import WebRow.Applets.Registration.Forms (passwordForm)
import WebRow.Crypto (Secret, sign, unsign)
import WebRow.Forms.Builders.Plain (Field, Repr) as Builder.Plain
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Plain (prefill', run) as Forms.Plain
import WebRow.Forms.Validation.Report (Result) as Forms.Validation.Report
import WebRow.Logging.Effect (LOGGER, info)
import WebRow.Logging.Effect as LogEff
import WebRow.Mailer (MAILER, sendMail)
import WebRow.Response (RESPONSE, badRequest'', methodNotAllowed, methodNotAllowed')
import WebRow.Response (response) as Response
import WebRow.Route (ROUTE)
import WebRow.Route (printFullRoute) as Route
import WebRow.Routing.Duplex (params) as WebRow.Routing.Duplex
import WebRow.Types (Email(..), Password, SignedEmail(..))

_register = SProxy ∷ SProxy "register"

type Namespace r t = (register ∷ t | r)

namespace ∷ ∀ a r. a → Variant (Namespace r a)
namespace = inj _register

data ConfirmationResponse
  = ConfirmationSucceeded Email Password
  | InvalidEmailSignature
  | InitialPasswordForm (Layout Builder.Plain.Field (Maybe Payload.Value) (Maybe (Forms.Validation.Report.Result Builder.Plain.Repr)))
  | EmailRegisteredInbetween Email
  | PasswordValidationFailed (Layout Builder.Plain.Field (Maybe Payload.Value) (Maybe (Forms.Validation.Report.Result Builder.Plain.Repr)))

data RegisterEmailResponse
  = EmailSent Email
  | EmailAlreadyRegistered Email

data Response
  = ConfirmationResponse ConfirmationResponse
  | RegisterEmailResponse RegisterEmailResponse

type ResponseRow r = Namespace r Response

response ∷ ∀ a eff res. Response → Run (response ∷ RESPONSE (ResponseRow res) | eff) a
response = Response.response <<< namespace

data Route
  = RegisterEmail Email
  | Confirmation SignedEmail
derive instance genericRoute ∷ Generic Route _

type RouteRow r = Namespace r Route

printFullRoute ∷ ∀ eff routes. Route → Run (route ∷ ROUTE (RouteRow routes) | eff) String
printFullRoute = Route.printFullRoute <<< namespace

data RegisterF a = CheckIfEmailRegistered Email (Either String Email → a)

derive instance functorRegisterF ∷ Functor RegisterF

type REGISTER = FProxy RegisterF

type EffectRow ctx res routes eff =
  ( aff ∷ AFF
  , logger ∷ LOGGER
  , mailer ∷ MAILER
  , reader ∷ READER { request ∷ HTTPure.Request, secret ∷ Secret | ctx }
  , register ∷ REGISTER
  , response ∷ RESPONSE (ResponseRow res)
  , route ∷ ROUTE (RouteRow routes)
  | eff
  )

duplexes ∷ { | Namespace () (D.RouteDuplex' Route) }
duplexes =
  { "register": DG.sum
    { "RegisterEmail": (_Newtype $ D.param "email" ∷ RouteDuplex' Email)
    , "Confirmation": "confirmation" / (_Newtype $ D.param "email" ∷ RouteDuplex' SignedEmail)
    }
  }

router
  ∷ ∀ eff ctx res routes a
  . (Variant routes → Run (EffectRow ctx res routes eff) a)
  → Variant (RouteRow routes)
  → Run (EffectRow ctx res routes eff) a
router = on _register $ case _ of
  RegisterEmail email → registerEmail email
  Confirmation email → confirmation email

confirmation
  ∷ ∀ eff ctx res a
  . SignedEmail
  → Run
      ( aff ∷ AFF
      , logger ∷ LOGGER
      , reader ∷ READER { request ∷ HTTPure.Request, secret ∷ Secret | ctx }
      , register ∷ REGISTER
      , response ∷ RESPONSE ( register ∷ Response | res )
      | eff
      )
      a
confirmation signedEmail = do
  email ← unsign (un SignedEmail signedEmail) >>= either onInvalidSig pure
  req ← ask <#> _.request
  case req.method of
    HTTPure.Post → do
      payload ← fromBody
      info $ unsafeStringify payload
      Forms.Plain.run passwordForm payload >>= case _ of
        o@{ result: Just password } →
          response $ ConfirmationResponse $ ConfirmationSucceeded (Email email) password
        o@{ layout } → do
          response $ ConfirmationResponse $ PasswordValidationFailed layout
    HTTPure.Get → do
      layout ← rmap (const Nothing) <$> Forms.Plain.prefill' passwordForm mempty
      response $ ConfirmationResponse $ InitialPasswordForm layout
    method → methodNotAllowed'
  where
    onInvalidSig err = response $ ConfirmationResponse $ InvalidEmailSignature


-- -- checkIfEmailTaken ∷ ∀ eff. Email → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) Boolean
-- validateEmail email = do
--   Run.lift _register (ValidateEmail email)

registerEmail
  ∷ ∀ eff ctx res routes a
  . Email
  → Run (EffectRow ctx res routes eff) a
registerEmail email = do
  -- checkIfEmailTaken email >>= flip when $
  --   response (RegisterEmailResponse $ EmailAlreadyRegistered email)
  signedEmail ← sign $ un Email email
  text ← printFullRoute $ Confirmation (SignedEmail signedEmail)
  _ ← sendMail { to: email, text, subject: "Email verification" }
  response $ RegisterEmailResponse $ EmailSent email



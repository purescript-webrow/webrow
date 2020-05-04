module WebRow.Applets.Registration where

import Prelude

import Control.Monad.Reader.Class (asks)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant, inj, on)
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
import WebRow.Crypto (Secret, sign, unsign)
import WebRow.Forms.Payload (fromBody)
import WebRow.Logging.Effect (LOGGER)
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
  | EmailRegisteredInbetween Email
  -- | PasswordValidationFailed (Widget (passwordInput ∷ TextInput Identity String))

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

data RegisterF a
  = ValidateEmail String (Either String Email → a)
  | ValidateConfirmation { email ∷ String }

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
  RegisterEmail email → response $ RegisterEmailResponse $ EmailAlreadyRegistered email -- registerEmail email
  Confirmation email → badRequest'' -- ConfirmationResponse $ EmailRegisteredInbetween email

confirmation
  ∷ ∀ eff ctx res a
  . SignedEmail
  → Run
      ( aff ∷ AFF
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
      badRequest''
      -- form ← fromBody <#> runValidator passwordForm
      -- case form of
      --   V (Right password) → response $ ConfirmationSucceeded email password
      --   V (Left form) → response $ PasswordValidationFailed email form
    HTTPure.Get → do
      badRequest''
      -- response $ PasswordValidationForm email Nothing
    method → methodNotAllowed'
  where
    onInvalidSig err = response $ ConfirmationResponse $ InvalidEmailSignature


-- -- checkIfEmailTaken ∷ ∀ eff. Email → Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) Boolean
-- validateEmail email = do
--   Run.lift _register (ValidateEmail email)
-- 
-- -- registerEmail
-- --   ∷ ∀ eff ctx res routes a
-- --   . Email
-- --   → Run (EffectRow ctx res routes eff) a
-- -- registerEmail email = do
-- --   checkIfEmailTaken email >>= flip when $
-- --     response (RegistrationFailure email)
-- -- 
-- --   signedEmail ← sign $ un Email email
-- --   text ← printFullRoute $ Confirmation { email: SignedEmail signedEmail, form: [] }
-- --   _ ← sendMail { to: email, text, subject: "Email verification" }
-- --   response $ EmailSent email
-- 
-- -- assertEmailNotTaken ∷
-- --   ∀ eff
-- --   . Email
-- --   → Run ( register ∷ REGISTER | eff ) Unit
-- -- assertEmailNotTaken email = Run.lift _register (AssertEmailNotTaken email unit)

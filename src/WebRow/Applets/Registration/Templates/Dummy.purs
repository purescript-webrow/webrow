module WebRow.Applets.Registration.Templates.Dummy where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Variant (on)
import Effect.Aff.Class (class MonadAff)
import Global.Unsafe (unsafeStringify)
import HTTPure as HTTPure
import Prim.Row as Row
import Run (AFF, Run, liftAff)
import Text.Smolder.HTML (a, p) as M
import Text.Smolder.HTML.Attributes (href) as A
import Text.Smolder.Markup (safe, (!))
import Text.Smolder.Markup (text) as M
import WebRow.Applets.Auth.Templates.Dummy (form, html)
import WebRow.Applets.Registration.Responses (ConfirmationResponse(..), RegisterEmailResponse(..), Response(..))
import WebRow.Applets.Registration.Types (_register)
import WebRow.Response (RESPONSE, interpretResponseWith, response)
import WebRow.Route (FullUrl(..))

-- | This is still dummy and unuseful approach
-- | Templates should be separated etc.
onRegister = on _register toHTTPureResponse

runResponseRegister ∷ forall eff res.
  Row.Union eff (response ∷ RESPONSE res) ( response ∷ RESPONSE res | eff )
  ⇒ Run
      ( response ∷ RESPONSE ( register ∷ Response | res )
      , aff ∷ AFF
      | eff
      )
      HTTPure.Response
  → Run
      ( response ∷ RESPONSE res
      , aff ∷ AFF
      | eff
      )
      HTTPure.Response
runResponseRegister = interpretResponseWith
  $ on _register (toHTTPureResponse >>> liftAff) response

toHTTPureResponse ∷ ∀ m. MonadAff m ⇒ Response → m HTTPure.Response
toHTTPureResponse = render >>> HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ])

render ∷ Response → String
render = case _ of
  RegisterEmailResponse r → case r of
    EmailValidationFailed formLayout → html $ form formLayout
    EmailSent email (FullUrl url) → html do
      M.p $
        M.text ("Email sent to " <> show email)
      M.p $ do
        M.text ("Activation link is: ")
        M.a ! (safe $ A.href url) $ M.text url
    InitialEmailForm formLayout → html $ form formLayout

  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
    EmailRegisteredInbetween _ → "Email registered inbetween"
    InitialPasswordForm formLayout → html $ form formLayout
    InvalidEmailSignature → "InvalidEmailSignature"
    PasswordValidationFailed formLayout → html $ form formLayout


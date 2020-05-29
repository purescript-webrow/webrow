module WebRow.Applets.Registration.Responses where

import Prelude

import Data.Maybe (Maybe)
import Run (Run)
import WebRow.Applets.Auth.Types (Password)
import WebRow.Applets.Registration.Types (Namespace, namespace)
import WebRow.Forms.Builders.Plain (Field, Repr) as Builder.Plain
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Validation.Report (Result) as Forms.Validation.Report
import WebRow.Mailer (Email)
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response
import WebRow.Route (FullUrl)

-- | Move to Forms package
type FormLayout = Layout Builder.Plain.Field (Maybe Payload.Value) (Maybe (Forms.Validation.Report.Result Builder.Plain.Repr))

data ConfirmationResponse
  = ConfirmationSucceeded Email Password
  | InvalidEmailSignature
  | InitialPasswordForm FormLayout
  | EmailRegisteredInbetween Email
  -- | TODO: expect whole form result
  | PasswordValidationFailed FormLayout

data RegisterEmailResponse
  = EmailSent Email FullUrl
  | EmailValidationFailed FormLayout
  | InitialEmailForm FormLayout

data Response
  = ConfirmationResponse ConfirmationResponse
  | RegisterEmailResponse RegisterEmailResponse

type ResponseRow r = Namespace r Response

response ∷ ∀ a eff res. Response → Run (response ∷ RESPONSE (ResponseRow res) | eff) a
response = Response.response <<< namespace


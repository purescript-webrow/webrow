module WebRow.Applets.Registration.Responses where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Run (Run)
import Type.Row (type (+))
import WebRow.Applets.Registration.Types (Namespace, Password, namespace)
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Plain (FieldRow, InputFieldRecord) as Forms.Plain
import WebRow.Forms.Validation.Report (Result) as Forms.Validation.Report
import WebRow.Mailer (Email)
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response
import WebRow.Route (FullUrl)

type FieldRow =
  (Forms.Plain.FieldRow String ( email :: Forms.Plain.InputFieldRecord String Email ))
 

-- | Move to Forms package
type FormLayout = Layout
  String
  (Variant FieldRow)

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


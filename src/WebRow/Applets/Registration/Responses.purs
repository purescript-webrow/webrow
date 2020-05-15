module WebRow.Applets.Registration.Responses where

import Prelude

import Data.Variant (Variant)
import Run (Run)
import WebRow.Applets.Registration.Types (Namespace, Password, namespace)
import WebRow.Forms.Fields (FieldRow, TextInputBase) as Forms.Fields
import WebRow.Forms.Layout (Layout)
import WebRow.Mailer (Email)
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response
import WebRow.Route (FullUrl)

type FieldRow msg =
 (Forms.Fields.FieldRow String ( email :: Forms.Fields.TextInputBase () String Email ))

-- | Move to Forms package
type FormLayout = Layout
  String
  (Variant (FieldRow String))

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

data ChangeEmailResponse
  = ChangeEmailInitialForm FormLayout

data Response
  = ConfirmationResponse ConfirmationResponse
  | RegisterEmailResponse RegisterEmailResponse
  | ChangeEmailResponse ChangeEmailResponse

type ResponseRow r = Namespace r Response

response ∷ ∀ a eff res. Response → Run (response ∷ RESPONSE (ResponseRow res) | eff) a
response = Response.response <<< namespace


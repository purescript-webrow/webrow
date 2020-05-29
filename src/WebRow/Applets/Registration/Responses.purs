module WebRow.Applets.Registration.Responses where

import Prelude

import Run (Run)
import WebRow.Applets.Auth.Responses (FormLayout)
import WebRow.Applets.Auth.Types (Password)
import WebRow.Applets.Registration.Types (Namespace, namespace)
import WebRow.Mailer (Email)
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response
import WebRow.Route (FullUrl)

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

type ResponseRow r = Namespace Response r

response ∷ ∀ a eff res. Response → Run (response ∷ RESPONSE (ResponseRow res) | eff) a
response = Response.response <<< namespace


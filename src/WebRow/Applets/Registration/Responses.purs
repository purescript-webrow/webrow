module WebRow.Applets.Registration.Responses where

import Prelude

import Run (Run)
import WebRow.Applets.Auth.Types (Password)
import WebRow.Applets.Registration.Types (Namespace, namespace)
import WebRow.Forms (Layout) as Forms
import WebRow.Mailer (Email)
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response
import WebRow.Route (FullUrl)

data ConfirmationResponse widgets
  = ConfirmationSucceeded Email Password
  | InvalidEmailSignature
  | InitialPasswordForm (Forms.Layout widgets)
  | EmailRegisteredInbetween Email
  | PasswordValidationFailed (Forms.Layout widgets)

data RegisterEmailResponse widgets
  = EmailSent Email FullUrl
  | EmailValidationFailed (Forms.Layout widgets)
  | InitialEmailForm (Forms.Layout widgets)

data ChangeEmailResponse widgets
  = ChangeEmailInitialForm (Forms.Layout widgets)

data Response widgets
  = ConfirmationResponse (ConfirmationResponse widgets)
  | RegisterEmailResponse (RegisterEmailResponse widgets)
  | ChangeEmailResponse (ChangeEmailResponse widgets)

type ResponseRow widgets r = Namespace r (Response widgets)

response ∷ ∀ a eff res widgets. Response widgets → Run (response ∷ RESPONSE (ResponseRow widgets res) | eff) a
response = Response.response <<< namespace

-- type FieldRow msg =
--  (Forms.Fields.FieldRow String ( email :: Forms.Fields.TextInputBase () String Email ))
-- 
-- -- | Move to Forms package
-- type FormLayout = Layout
--   String
--   (FieldRow String)
-- 

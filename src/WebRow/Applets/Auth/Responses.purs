module WebRow.Applets.Auth.Responses where

import Prelude

import Data.Maybe (Maybe)
import Run (Run)
import WebRow.Applets.Auth.Types (namespace)
import WebRow.Forms.Builders.Plain (Field, Repr) as Builder.Plain
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Validation.Report (Result) as Forms.Validation.Report
import WebRow.Response (RESPONSE)
import WebRow.Response (response) as Response

-- | Move to Forms package
type FormLayout = Layout Builder.Plain.Field (Maybe Payload.Value) (Maybe (Forms.Validation.Report.Result Builder.Plain.Repr))

data LoginResponse
  = LoginFormValidationFailed FormLayout
  | EmailPasswordMismatch FormLayout
  | InitialEmailPassordForm FormLayout
  | LoginSuccess

data Response
  = LoginResponse LoginResponse

response
  ∷ ∀ a eff res
  . Response
  → Run (response ∷ RESPONSE ( auth ∷ Response | res ) | eff) a
response = Response.response <<< namespace

type LayoutResponse
  = ∀ res eff a
  . FormLayout
  → Run (response ∷ RESPONSE ( auth ∷ Response | res ) | eff) a

loginFormValidationFailed ∷ LayoutResponse
loginFormValidationFailed = response <<< LoginResponse <<< LoginFormValidationFailed

emailPasswordMismatch ∷ LayoutResponse
emailPasswordMismatch = response <<< LoginResponse <<< EmailPasswordMismatch

initialEmailPassordForm ∷ LayoutResponse
initialEmailPassordForm = response <<< LoginResponse <<< InitialEmailPassordForm

loginSuccess
  ∷ ∀ res eff a
  . Run (response ∷ RESPONSE ( auth ∷ Response | res ) | eff) a
loginSuccess = response $ LoginResponse LoginSuccess

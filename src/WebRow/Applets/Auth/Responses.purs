module WebRow.Applets.Auth.Responses where

import WebRow.Applets.Auth.Forms (LoginLayout) as Forms
import WebRow.Applets.Auth.Types (Namespace)

data LoginResponse
  = LoginFormValidationFailed Forms.LoginLayout
  | EmailPasswordMismatch Forms.LoginLayout
  | InitialEmailPassordForm Forms.LoginLayout
  | LoginSuccess

data Response
  = LoginResponse LoginResponse
  | LogoutResponse

type ResponseRow responses
  = Namespace Response responses

-- response
--   ∷ ∀ a eff res widgets
--   . Response widgets
--   → Run (response ∷ RESPONSE ( auth ∷ Response widgets | res ) | eff) a
-- response = Response.response <<< namespace
-- 
-- type LayoutResponse widgets
--   = ∀ res eff a widgets
--   . (Forms.Layout widgets)
--   → Run (response ∷ RESPONSE ( auth ∷ (Response widgets) | res ) | eff) a
-- loginFormValidationFailed ∷ forall widgets. Response widgets
-- loginFormValidationFailed = LoginResponse <<< LoginFormValidationFailed
-- 
-- emailPasswordMismatch ∷ forall widgets. Response widgets
-- emailPasswordMismatch = LoginResponse <<< EmailPasswordMismatch
-- 
-- initialEmailPassordForm ∷ forall widgets. Response widgets
-- initialEmailPassordForm = response <<< LoginResponse <<< InitialEmailPassordForm
-- 
-- loginSuccess
--   ∷ ∀ res eff a widgets
--   . Run (response ∷ RESPONSE ( auth ∷ Response widgets | res ) | eff) a
-- loginSuccess = response $ LoginResponse LoginSuccess

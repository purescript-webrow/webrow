module WebRow.Applets.Auth.Responses where

import Prelude

import Run (Run)
import WebRow.Applets.Auth.Types (namespace)
import WebRow.Forms (Layout) as Forms

data LoginResponse (widgets ∷ # Type)
  = LoginFormValidationFailed (Forms.Layout widgets)
  | EmailPasswordMismatch (Forms.Layout widgets)
  | InitialEmailPassordForm (Forms.Layout widgets)
  | LoginSuccess

data Response (widgets ∷ # Type)
  = LoginResponse (LoginResponse widgets)

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

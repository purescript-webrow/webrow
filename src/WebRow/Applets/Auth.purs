module WebRow.Applets.Auth where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (on)
import HTTPure as HTTPure
import WebRow.Applets.Auth.Forms (emailPassordForm)
import WebRow.Applets.Auth.Routes as Routes
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration.Types (Password(..))
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Plain as Forms.Plain
import WebRow.Mailer (Email(..))
import WebRow.Reader (request)

-- router = on _auth case _ of
--   Routes.Login → onLoginRoute

-- onLoginRoute = request >>= _.method >>> case _ of
--   HTTPure.Post → do
--     body ← fromBody
--     Forms.Plain.run emailPassordForm body >>= case _ of
--       Left { layout } → do
--         Responses.loginFormValidationFailed layout
--       Right { result: { email, password }, layout } → do
--         Effects.authenticate (Email email) (Password password) >>= case _ of
--           Nothing → Responses.emailPasswordMismatch layout
--           Just user → 

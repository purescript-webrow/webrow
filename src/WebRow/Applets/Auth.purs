module WebRow.Applets.Auth where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, on)
import HTTPure as HTTPure
import Run (Run)
import WebRow.Applets.Auth.Effects (AUTH)
import WebRow.Applets.Auth.Effects as Effects
import WebRow.Applets.Auth.Forms (emailPassordForm)
import WebRow.Applets.Auth.Responses as Responses
import WebRow.Applets.Auth.Routes as Routes
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Applets.Registration.Types (Password(..))
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Plain as Forms.Plain
import WebRow.Mailer (Email(..))
import WebRow.Reader (request)
import WebRow.Reader as WebRow.Reader
import WebRow.Response (RESPONSE, methodNotAllowed')
import WebRow.Session.Effect (SESSION)
import WebRow.Session.Effect as Session

router
  ∷ ∀ a res ctx session eff routes
  . (Variant routes → Run (Effects session ctx res eff) a)
  → Variant ( auth ∷ Routes.Route | routes )
  → Run (Effects session ctx res eff) a
router = on _auth case _ of
  Routes.Login → onLoginRoute

onLoginRoute
  ∷ ∀ a res ctx session eff
  . Run (Effects session ctx res eff) a
onLoginRoute = request >>= _.method >>> case _ of
  HTTPure.Post → do
    body ← fromBody
    Forms.Plain.run emailPassordForm body >>= case _ of
      Left { layout } → Responses.loginFormValidationFailed layout
      Right { result: { email, password }, layout } → do
        -- move to form validation
        Effects.authenticate email (Password password) >>= case _ of
          Nothing → Responses.emailPasswordMismatch layout
          Just _ → do
            Session.modify \s → s { user = Just email }
            Responses.loginSuccess
  HTTPure.Get → do
    layout ← Forms.Plain.prefill' emailPassordForm mempty
    Responses.initialEmailPassordForm layout
  method → methodNotAllowed'

type Effects session ctx res eff =
  ( auth ∷ AUTH
  , reader ∷ WebRow.Reader.READER ctx
  , response ∷ RESPONSE ( auth ∷ Responses.Response | res )
  , session ∷ SESSION { user ∷ Maybe Email | session }
  | eff
  )

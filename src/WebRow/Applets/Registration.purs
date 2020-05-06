module WebRow.Applets.Registration where

import Prelude

import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Variant (Variant, on)
import HTTPure (Method(..)) as HTTPure
import Run (Run)
import WebRow.Applets.Registration.Effects (Effects)
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Applets.Registration.Forms (emailForm, passwordForm)
import WebRow.Applets.Registration.Responses (ConfirmationResponse(..), RegisterEmailResponse(..), Response(..)) as Responses
import WebRow.Applets.Registration.Responses (response)
import WebRow.Applets.Registration.Routes (Route(..), RouteRow, printFullRoute) as Routes
import WebRow.Applets.Registration.Types (SignedEmail(..), _register)
import WebRow.Crypto (sign, unsign)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Plain (prefill', run) as Forms.Plain
import WebRow.Logging.Effect (info)
import WebRow.Mailer (Email(..), sendMail)
import WebRow.Reader (request)
import WebRow.Response (methodNotAllowed')
import WebRow.Route (FullUrl(..))

-- | I'm not sure about this response polymorphism here
-- | Is it good or bad? Or doesn't matter at all?
router
  ∷ ∀ a eff ctx res routes
  . (Variant routes → Run (Effects ctx res routes eff) a)
  → Variant (Routes.RouteRow routes)
  → Run (Effects ctx res routes eff) a
router = on _register $ case _ of
  Routes.RegisterEmail → registerEmail
  Routes.Confirmation email → confirmation email

registerEmail
  ∷ ∀ a eff ctx res routes
  . Run (Effects ctx res routes eff) a
registerEmail = request >>= _.method >>> case _ of
  HTTPure.Post → fromBody >>= Forms.Plain.run emailForm >>= case _ of
    Right { result: email@(Email e) } → do
      signedEmail ← sign e
      fullUrl@(FullUrl url) ← Routes.printFullRoute $ Routes.Confirmation (SignedEmail signedEmail)
      void $ sendMail { to: email, text: "Verification link" <> url, subject: "Email verification" }
      response $ Responses.RegisterEmailResponse $ Responses.EmailSent email fullUrl
    Left { layout } → do
      response $ Responses.RegisterEmailResponse $ Responses.EmailValidationFailed layout
  HTTPure.Get → do
    layout ← rmap (const Nothing) <$> Forms.Plain.prefill' emailForm mempty
    response $ Responses.ConfirmationResponse $ Responses.InitialPasswordForm layout
  method → methodNotAllowed'

confirmation
  ∷ ∀ a eff ctx res routes
  . SignedEmail
  → Run (Effects ctx res routes eff) a
confirmation signedEmail = do
  info $ show signedEmail
  email ← unsign (un SignedEmail signedEmail) >>= either onInvalidSig (Email >>> pure)
  Effects.emailTaken email >>= flip when do
    response $ Responses.ConfirmationResponse $ Responses.EmailRegisteredInbetween email
  request >>= _.method >>> case _ of
    HTTPure.Post → do
      payload ← fromBody
      Forms.Plain.run passwordForm payload >>= case _ of
        Right { result: password } →
          response $ Responses.ConfirmationResponse $ Responses.ConfirmationSucceeded email password
        Left { layout } → do
          response $ Responses.ConfirmationResponse $ Responses.PasswordValidationFailed layout
    HTTPure.Get → do
      layout ← Forms.Plain.prefill' passwordForm mempty
      response $ Responses.ConfirmationResponse $ Responses.InitialPasswordForm layout
    method → methodNotAllowed'
  where
    onInvalidSig err = response $ Responses.ConfirmationResponse $ Responses.InvalidEmailSignature



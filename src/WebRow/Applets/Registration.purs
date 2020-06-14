module WebRow.Applets.Registration where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on)
import Global.Unsafe (unsafeStringify)
import HTTPure (Method(..)) as HTTPure
import Polyform.Dual (dual)
import Run (Run)
import Simple.JSON (readJSON, writeJSON)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Auth.Effects (AUTH)
import WebRow.Applets.Auth.Effects (AUTH) as Auth.Effects
import WebRow.Applets.Auth.Routes (RouteRow) as Auth
import WebRow.Applets.Auth.Routes (RouteRow) as Auth.Routes
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Applets.Registration.Forms (emailTakenForm, passwordForm)
import WebRow.Applets.Registration.Responses (ChangeEmailResponse(..), ConfirmationResponse(..), RegisterEmailResponse(..), Response(..)) as Responses
import WebRow.Applets.Registration.Routes (Route(..), RouteRow, printFullRoute) as Routes
import WebRow.Applets.Registration.Types (SignedEmail(..), _register)
import WebRow.Crypto (sign, unsign)
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.HTTPError (methodNotAllowed')
import WebRow.Logging.Effect (info)
import WebRow.Mailer (Email(..), send)
import WebRow.Mailer (send) as Mailer
import WebRow.Request (method)
import WebRow.Route (FullUrl(..))

-- -- | I'm not sure about this response polymorphism here
-- -- | Is it good or bad? Or doesn't matter at all?
-- router
--   ∷ ∀ a eff ctx res routes user
--   . (Variant (Auth.Routes.RouteRow + routes) → Run (Effects ctx res routes user eff) a)
--   → Variant (Routes.RouteRow + Auth.Routes.RouteRow + routes)
--   → Run (Effects ctx res routes user eff) a
router = on _register $ case _ of
    Routes.RegisterEmail → registerEmail
    Routes.Confirmation email → confirmation email
--     Routes.ChangeEmail → changeEmail
--     Routes.ChangeEmailConfirmation payload → changeEmailConfirmation payload

_emailVerification = SProxy ∷ SProxy "emailVerification"

-- registerEmail
--   ∷ ∀ a eff ctx res routes user widgets
--   . Run (Effects ctx res routes user widgets eff) a
registerEmail = method >>= case _ of
  HTTPure.Post → fromBody >>= Forms.Uni.validate emailTakenForm >>= case _ of
    Tuple form (Just email@(Email e)) → do
      signedEmail ← sign e
      confirmationLink ← Routes.printFullRoute $ Routes.Confirmation (SignedEmail signedEmail)
      void $ Mailer.send ({ to: email, context: inj _emailVerification confirmationLink })
      pure $ Responses.RegisterEmailResponse $ Responses.EmailSent email confirmationLink
    Tuple form _ → do
      pure $ Responses.RegisterEmailResponse $ Responses.EmailValidationFailed form
  HTTPure.Get → do
    form ← Forms.Uni.default emailTakenForm
    pure $ Responses.ConfirmationResponse $ Responses.InitialPasswordForm form
  method → methodNotAllowed'

-- confirmation
--   ∷ ∀ a eff ctx res routes user
--   . SignedEmail
--   → Run (Effects ctx res routes user eff) a
confirmation signedEmail = do
  validateEmail signedEmail >>= case _ of
    Left err → pure err
    Right email → method >>= case _ of
      HTTPure.Post → fromBody >>= Forms.Uni.validate passwordForm >>= case _ of
        Tuple _ (Just password) →
          pure $ Responses.ConfirmationResponse $ Responses.ConfirmationSucceeded email password
        Tuple form _ → do
          pure $ Responses.ConfirmationResponse $ Responses.PasswordValidationFailed form
      HTTPure.Get → do
        form ← Forms.Uni.default passwordForm
        pure $ Responses.ConfirmationResponse $ Responses.InitialPasswordForm form
      _ → methodNotAllowed'
  where
    validateEmail = un SignedEmail >>> unsign >=> case _ of
      Left _ → pure $ Left (Responses.ConfirmationResponse $ Responses.InvalidEmailSignature)
      Right emailStr → do
        let
          email = Email emailStr
        Effects.emailTaken email >>= if _
          then pure $ Left (Responses.ConfirmationResponse $ Responses.EmailRegisteredInbetween email)
          else pure $ Right email

-- -- type ChangeEmailPayload = { current ∷ String, new ∷ String }
-- -- 
-- -- changeEmail
-- --   ∷ ∀ a eff ctx res routes user
-- --   . Run
-- --     (Effects ctx res routes user eff)
-- --     a
-- -- changeEmail = do
-- --   { email: current@(Email c) } ← userRequired
-- --   request >>= _.method >>> case _ of
-- --     HTTPure.Post → fromBody >>= Forms.Dual.run (updateEmailForm current) >>= case _ of
-- --       Tuple form (Just email@(Email e)) → do
-- --         signedPayload ← sign $ writeJSON { current: c, new: e }
-- --         fullUrl@(FullUrl url) ← Routes.printFullRoute $ Routes.ChangeEmailConfirmation { payload: signedPayload }
-- --         void $ sendMail { to: email, text: "Verification link for email change" <> url, subject: "Email verification" }
-- --         -- | TODO: Fix response
-- --         response $ Responses.RegisterEmailResponse $ Responses.EmailSent email fullUrl
-- --       Tuple form _ → do
-- --         -- | TODO: Fix response
-- --         response $ Responses.RegisterEmailResponse $ Responses.EmailValidationFailed form
-- --     HTTPure.Get → do
-- --       form ← Forms.Dual.build (updateEmailForm current) current
-- --       response $ Responses.ChangeEmailResponse $ Responses.ChangeEmailInitialForm form
-- --     method → methodNotAllowed'
-- -- 
-- -- changeEmailConfirmation
-- --   ∷ ∀ a eff ctx res routes user
-- --   . { payload ∷ String }
-- --   → Run
-- --     (Effects ctx res routes user eff)
-- --     a
-- -- changeEmailConfirmation { payload } = do
-- --   (p@{ current: currentEmail, new: newEmail } ∷ ChangeEmailPayload) ←
-- --     maybe (badRequest' "Bad signature") pure
-- --     <<< ((hush <<< readJSON) <=< hush)
-- --     <=< unsign
-- --     $ payload
-- --   Effects.emailTaken (Email newEmail) >>= flip when do
-- --     response $ Responses.ConfirmationResponse $ Responses.EmailRegisteredInbetween (Email newEmail)
-- -- 
-- --   badRequest' $ "CORRECTLY PARSED PAYLOAD: " <> writeJSON p
-- -- 
-- --   -- request >>= _.method >>> case _ of
-- --   --   HTTPure.Post → fromBody >>= Forms.Dual.run updateEmailForm >>= case _ of
-- --   --     Tuple form (Just email@(Email e)) → do
-- --   --       signedPayload ← sign $ writeJSON { current: c, new: e }
-- --   --       fullUrl@(FullUrl url) ← Routes.printFullRoute $ Routes.ChangeEmailConfirmation { payload: signedPayload }
-- --   --       void $ sendMail { to: email, text: "Verification link for email change" <> url, subject: "Email verification" }
-- --   --       response $ Responses.RegisterEmailResponse $ Responses.EmailSent email fullUrl
-- --   --     Tuple form _ → do
-- --   --       response $ Responses.RegisterEmailResponse $ Responses.EmailValidationFailed form
-- --   --   HTTPure.Get → do
-- --   --     let
-- --   --       form = Forms.Dual.build updateEmailForm current
-- --   --     response $ Responses.ChangeEmailResponse $ Responses.ChangeEmailInitialForm form
-- --   --   method → methodNotAllowed'

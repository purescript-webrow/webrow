module WebRow.Applets.Registration where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Variant (inj, on)
import HTTPure (Method(..)) as HTTPure
import Run (Run)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Applets.Registration.Effects (Registration)
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Applets.Registration.Forms (emailTakenForm, passwordForm)
import WebRow.Applets.Registration.Responses (ConfirmationResponse(..), RegisterEmailResponse(..), Response(..))
import WebRow.Applets.Registration.Routes (Route(..), printFullRoute) as Routes
import WebRow.Applets.Registration.Routes (Route) as Registration
import WebRow.Applets.Registration.Types (SignedEmail(..), _register)
import WebRow.Contrib.Run (EffRow)
import WebRow.Crypto (Crypto, sign, unsign)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Payload (fromBody)
import WebRow.Forms.Uni (default, validate) as Forms.Uni
import WebRow.HTTP (methodNotAllowed')
import WebRow.Mailer (Email(..), Mailer)
import WebRow.Mailer (send) as Mailer
import WebRow.Request (method)
import WebRow.Route (FullUrl)
import WebRow.Types (WebRow)

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

registerEmail
  :: ∀ eff mails messages routes session
  . Run
    ( Crypto
    + EffRow
    + Mailer (emailVerification ∷ FullUrl | mails)
    + Registration
    + WebRow
      ( emailTaken :: Email
      , invalidEmailFormat :: String
      , singleValueExpected :: Maybe (Array String)
      | messages
      )
      session
      (register :: Registration.Route | routes)
    + eff
    )
    Response
registerEmail = method >>= case _ of
  HTTPure.Post → fromBody >>= Forms.Uni.validate emailTakenForm >>= case _ of
    Tuple form (Just email@(Email e)) → do
      signedEmail ← sign e
      confirmationLink ← Routes.printFullRoute $ Routes.Confirmation (SignedEmail signedEmail)
      void $ Mailer.send ({ to: email, context: inj _emailVerification confirmationLink })
      pure $ RegisterEmailResponse $ EmailSent email confirmationLink
    Tuple form _ → do
      pure $ RegisterEmailResponse $ EmailValidationFailed form
  HTTPure.Get → do
    form ← Forms.Uni.default emailTakenForm
    pure $ ConfirmationResponse $ InitialPasswordForm form
  method → methodNotAllowed'

confirmation
  :: ∀ eff messages routes session
  . SignedEmail
  → Run
    ( Crypto
    + EffRow
    + Registration
    + WebRow
      ( singleValueExpected :: Maybe Payload.Value
      , passwordsMismatch :: { password1 ∷ String, password2 ∷ String }
      | messages
      )
      session
      routes
    + eff
    )
    Response
confirmation signedEmail = do
  validateEmail signedEmail >>= case _ of
    Left err → pure err
    Right email → method >>= case _ of
      HTTPure.Post → fromBody >>= Forms.Uni.validate passwordForm >>= case _ of
        Tuple _ (Just password) →
          pure $ ConfirmationResponse $ ConfirmationSucceeded email password
        Tuple form _ → do
          pure $ ConfirmationResponse $ PasswordValidationFailed form
      HTTPure.Get → do
        form ← Forms.Uni.default passwordForm
        pure $ ConfirmationResponse $ InitialPasswordForm form
      _ → methodNotAllowed'
  where
    validateEmail = un SignedEmail >>> unsign >=> case _ of
      Left _ → pure $ Left (ConfirmationResponse InvalidEmailSignature)
      Right emailStr → do
        let
          email = Email emailStr
        Effects.emailTaken email >>= if _
          then pure $ Left (ConfirmationResponse $ EmailRegisteredInbetween email)
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

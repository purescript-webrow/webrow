module WebRow.Applets.Registration.Forms where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains) as String
import Data.Validation.Semigroup (invalid)
import Data.Variant (inj)
import Polyform.Dual (dual) as Duals.Validator
import Polyform.Reporter (hoistFnEither)
import Polyform.Validator (hoistFnEither, hoistFnMV, hoistFnV) as Validator
import Polyform.Validator (valid)
import Polyform.Validators.UrlEncoded (string)
import Polyform.Validators.UrlEncoded (string) as Validators
import Type.Prelude (SProxy(..))
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Applets.Registration.Types (Password(..))
import WebRow.Forms.Dual (textInput) as Forms.Dual
import WebRow.Forms.Plain (input, passwordField, sectionValidator, textInput) as Forms.Plain
import WebRow.Mailer (Email(..))

-- | TODO: Move this to polyform validators
nonEmptyStringValidator = string >>> Validator.hoistFnEither \p → case p of
  "" → Left ["Value is required"]
  otherwise → Right p

nonEmptyStringDual = Duals.Validator.dual
  nonEmptyStringValidator
  (\s → pure (Just [s]))

-- | TODO: Fix this validator - possibly use this:
-- |
-- | https://github.com/cdepillabout/purescript-email-validate
-- |
-- | and push this validator to polyform-validators
emailFormatValidator = Validator.hoistFnV \email →
  -- | @ is just enough for as to send an email ;-)
  if String.contains (String.Pattern "@") email
    then valid (Email email)
    else invalid [ "Invalid email format: " <> email ]

emailFormatDual = Duals.Validator.dual
  emailFormatValidator
  (\(Email email) → pure email)


emailTakenValidator = Validator.hoistFnMV \email → do
  Effects.emailTaken email >>= if _
    then pure $ invalid [ "Email already taken:" <> show email ]
    else pure $ valid email

_email = SProxy ∷ SProxy "email"

emailForm = Forms.Plain.input "" _email validator
  where
    validator = nonEmptyStringValidator >>> emailFormatValidator >>> emailTakenValidator

passwordForm = passwordsForm >>> Forms.Plain.sectionValidator validator
  where
    validator = Validator.hoistFnEither \{ password1, password2 } → if password1 /= password2
      then
        Left "Passwords don't match"
      else
        Right (Password password1)

    passwordsForm = { password1: _, password2: _ }
      <$> Forms.Plain.passwordField
      <*> Forms.Plain.passwordField

updateEmailForm = Forms.Dual.textInput "" "email" dual
  where
    dual = nonEmptyStringDual >>> emailFormatDual

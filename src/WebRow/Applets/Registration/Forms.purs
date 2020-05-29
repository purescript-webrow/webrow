module WebRow.Applets.Registration.Forms where

import Prelude

import Data.Either (Either(..))
import Data.String (Pattern(..), contains) as String
import Data.Validation.Semigroup (invalid)
import Polyform.Validator (hoistFnEither, hoistFnMV, hoistFnV) as Validator
import Polyform.Validator (valid)
import Polyform.Validators.UrlEncoded (string)
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Applets.Registration.Effects (emailTaken) as Effects
import WebRow.Forms.Builders.Plain (field) as Forms.Builders.Plain
import WebRow.Forms.Builders.Plain (passwordField, sectionValidator)
import WebRow.Mailer (Email(..))

-- | TODO: Move this to polyform validators
nonEmptyString = string >>> Validator.hoistFnEither \p → case p of
  "" → Left ["Value is required"]
  otherwise → Right p

-- | TODO: Fix this validator - possibly use this:
-- |
-- | https://github.com/cdepillabout/purescript-email-validate
-- |
-- | and push this validator to polyform-validators
emailFormat = Validator.hoistFnV \email →
  -- | @ is just enough for as to send an email ;-)
  if String.contains (String.Pattern "@") email
    then valid (Email email)
    else invalid [ "Invalid email format: " <> email ]

emailTaken = Validator.hoistFnMV \email → do
  Effects.emailTaken email >>= if _
    then pure $ invalid [ "Email already taken:" <> show email ]
    else pure $ valid email

emailForm = Forms.Builders.Plain.field { name: "email", type_: "email" } validator
  where
    validator = nonEmptyString >>> emailFormat >>> emailTaken

passwordForm = passwordsForm >>> sectionValidator "no-match" validator
  where
    validator = Validator.hoistFnEither \{ password1, password2 } → if password1 /= password2
      then
        Left ["Passwords don't match"]
      else
        Right (Password password1)

    passwordsForm = { password1: _, password2: _ }
      <$> passwordField "password1" nonEmptyString
      <*> passwordField "password2" nonEmptyString

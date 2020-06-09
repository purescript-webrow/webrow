module WebRow.Applets.Auth.Forms where

import Prelude

import Polyform.Batteries.UrlEncoded.Validators (SingleValueExpected)
import Polyform.Batteries.UrlEncoded.Validators (singleValue) as Batteries
import Type.Row (type (+))
import WebRow.Forms (Uni) as Forms
import WebRow.Forms.Uni (build, passwordInputBuilder, textInputBuilder) as Uni
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Validators (email) as Validators
import WebRow.Forms.Widgets (TextInput)
import WebRow.Mailer (Email)

emailPassordForm :: forall eff errs widgets.
  Forms.Uni
    eff
    ( InvalidEmailFormat + SingleValueExpected + errs)
    ( TextInput + widgets)
    { email :: Email
    , password :: String
    }
emailPassordForm = Uni.build $ { email: _, password: _ } <$> emailFormBuilder <*> passwordFormBuilder
  where
    passwordFormBuilder = Uni.passwordInputBuilder {}
    emailFormBuilder = Uni.textInputBuilder
      { type_: "email"
      , validator: Batteries.singleValue >>> Validators.email
      }

-- -- | This is bidirectional form which serializer is not isomoprhic on purpose ;-)
-- -- passwordConfirmation email =
-- 
-- _password = SProxy ∷ SProxy "password"
-- 
-- -- passwordFormBuilder email = Forms.Bi.textInputBuilder "" "password" dual
-- --   where
-- --     dual = Dual.dual validator serializer
-- --     -- | We don't want to prefill password field ;-)
-- --     serializer = const $ pure (Just [""])
-- --     validator = UrlEncoded.Validators.singleValue >>> String.Validators.isNotEmpty >>> Validator.checkM
-- --         (Batteries.error _password)
-- --         (Effects.checkPassword email <<< Password)

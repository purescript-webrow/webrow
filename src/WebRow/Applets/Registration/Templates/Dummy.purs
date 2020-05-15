module WebRow.Applets.Registration.Templates.Dummy where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Variant (case_, on)
import Global.Unsafe (unsafeStringify)
import HTTPure (headers, ok') as HTTPure
import Text.Smolder.HTML (Html)
import Text.Smolder.HTML (a, div, form, h2, html, input, p) as M
import Text.Smolder.HTML.Attributes (href, lang, method, name, type', value) as A
import Text.Smolder.Markup (safe, (!))
import Text.Smolder.Markup (text) as M
import Text.Smolder.Renderer.String (render) as S
import WebRow.Applets.Registration.Forms (_email)
import WebRow.Applets.Registration.Responses (ChangeEmailResponse(..), ConfirmationResponse(..), FormLayout, RegisterEmailResponse(..), Response(..))
import WebRow.Applets.Registration.Types (_register)
import WebRow.Forms.Fields (_textInput)
import WebRow.Forms.Layout (Layout(..))
import WebRow.Route (FullUrl(..))

-- | This is still dummy and unuseful approach
-- | Templates should be separated etc.
onRegister = on _register (render >>> HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ]))

-- | Basic form renderer
html ∷ Html Unit → String
html body = S.render $ M.html ! A.lang "en" $ body

formBody ∷ FormLayout → Html Unit
formBody (Section { closed, errors, layout }) = do
  for_ errors \err →
    M.p $ M.text (unsafeStringify err)
  case closed of
    Just { title } → M.h2 $ M.text title
    Nothing → pure unit
  for_ layout formBody
formBody (Field field) = c field
  where
    c = case_
      # on _textInput formField
      # on _email formField

-- ( "textInput" ∷ { name ∷ String, input ∷ Maybe Payload.Value, result ∷ Maybe (Either (Array msg) String), type_ ∷ String }

formField { input, name, result, type_ } = M.div $ do
  case result of
    Just (Left r) → M.text (unsafeStringify r)
    otherwise → pure unit
  M.input ! A.type' type_ ! A.name name ! A.value (fromMaybe "" (input >>= Array.head))

form ∷ FormLayout → Html Unit
form l = do
  M.form ! A.method "post" $ do
    formBody l
    M.input ! A.type' "submit" ! A.value "submit"

render ∷ Response → String
render = case _ of
  RegisterEmailResponse r → case r of
    EmailValidationFailed formLayout → html $ form formLayout
    EmailSent email (FullUrl url) → html do
      M.p $
        M.text ("Email sent to " <> show email)
      M.p $ do
        M.text ("Activation link is: ")
        M.a ! (safe $ A.href url) $ M.text url
    InitialEmailForm formLayout → html $ form formLayout

  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
    EmailRegisteredInbetween _ → "Email registered inbetween"
    InitialPasswordForm formLayout → html $ form formLayout
    InvalidEmailSignature → "InvalidEmailSignature"
    PasswordValidationFailed formLayout → html $ form formLayout

  ChangeEmailResponse r → case r of
    ChangeEmailInitialForm formLayout → html $ form formLayout

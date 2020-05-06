module WebRow.Applets.Registration.Templates.Dummy where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Variant (on)
import Global.Unsafe (unsafeStringify)
import HTTPure (header, headers, ok, ok') as HTTPure
import Run (AFF, Run(..))
import Text.Smolder.HTML (div, form, h2, html, input) as M
import Text.Smolder.HTML.Attributes (action, lang, method, name, type', value) as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as M
import Text.Smolder.Renderer.String (render) as S
import WebRow.Applets.Registration (ConfirmationResponse(..), RegisterEmailResponse(..), _register)
import WebRow.Applets.Registration (Response(..))
import WebRow.Forms.Builders.Plain (Field(..)) as Forms.Builder
import WebRow.Forms.Layout (Layout(..))
import WebRow.Logging.Effect (LOGGER)

-- | This is still dummy and unuseful approach
-- | Templates should be separated etc.
onRegister = on _register (render >>> HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ]))

-- | Basic form renderer
html body = S.render $ M.html ! A.lang "en" $ body

formBody (Section { closed, layout, reports }) = do
  M.text $ unsafeStringify reports
  -- case closed of
  --   Just { title } → M.h2 $ M.text title
  --   Nothing → pure unit
  for_ layout formBody

formBody (Field ({ field: Forms.Builder.InputField { name, type_ }, input: value, result })) = M.div $ do
  M.text $ unsafeStringify result
  -- case result of
  --   Just (Left r) → M.text (unsafeStringify r)
  --   otherwise → pure unit
  M.input ! A.type' type_ ! A.name name ! A.value (fromMaybe "" (value >>= Array.head))

form l = do
  M.form ! A.method "post" $ do
    formBody l
    M.input ! A.type' "submit" ! A.value "submit"

render = case _ of
  RegisterEmailResponse r → case r of
    EmailAlreadyRegistered email → "Email already registered:" <> show email
    EmailSent email → "Email sent to " <> show email

  ConfirmationResponse r → case r of
    ConfirmationSucceeded email password → "email: " <> unsafeStringify email <> "; password: " <> unsafeStringify password
    EmailRegisteredInbetween _ → "Email registered inbetween"
    InitialPasswordForm formLayout → html $ do
      form formLayout
    InvalidEmailSignature → "InvalidEmailSignature"
    PasswordValidationFailed formLayout → html $ do
      form formLayout


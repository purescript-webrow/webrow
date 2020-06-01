module WebRow.Applets.Auth.Templates.Dummy where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Variant (on)
import Global.Unsafe (unsafeStringify)
import HTTPure as HTTPure
import Text.Smolder.HTML (Html)
import Text.Smolder.HTML (div, form, h2, html, input, p) as M
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as M
import Text.Smolder.Renderer.String as S
import WebRow.Applets.Auth.Responses (FormLayout, LoginResponse(..), Response(..))
import WebRow.Applets.Auth.Types (_auth)
import WebRow.Forms.Builders.Plain as Forms.Builder
import WebRow.Forms.Layout (Layout(..))

-- | This is still dummy and unuseful approach
-- | Templates should be separated etc.
onAuth = on _auth case _ of
  LoginResponse loginResponse → case loginResponse of
    LoginFormValidationFailed formLayout → ok $ html $ form formLayout
    EmailPasswordMismatch formLayout → ok $ html $ form formLayout
    InitialEmailPassordForm formLayout → ok $ html $ form formLayout
    LoginSuccess → HTTPure.temporaryRedirect "http://localhost:8080"
  where ok = HTTPure.ok' (HTTPure.headers [ Tuple "content-type" "text/html" ])

-- | Basic form renderer
html ∷ Html Unit → String
html body = S.render $ M.html ! A.lang "en" $ body

formBody ∷ FormLayout → Html Unit
formBody (Section { closed, layout, reports }) = do
  for_ reports case _ of
    Just (Left r) → M.p $ M.text (unsafeStringify r)
    otherwise → pure unit
  case closed of
    Just { title } → M.h2 $ M.text title
    Nothing → pure unit
  for_ layout formBody

formBody (Field ({ field: Forms.Builder.InputField { name, type_ }, input: value, result })) = M.div $ do
  case result of
    Just (Left r) → M.text (unsafeStringify r)
    otherwise → pure unit
  M.input ! A.type' type_ ! A.name name ! A.value (fromMaybe "" (value >>= Array.head))

form ∷ FormLayout → Html Unit
form l = do
  M.form ! A.method "post" $ do
    formBody l
    M.input ! A.type' "submit" ! A.value "submit"
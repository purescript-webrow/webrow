module WebRow.Testing.Templates where

import Prelude

import Data.Array (head) as Array
import Data.Foldable (for_)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant (case_, on) as VariantF
import Data.Lazy (force) as Lazy
import Data.Maybe (Maybe(..), fromMaybe)
import Polyform.Batteries (Msg)
import Text.Smolder.HTML (Html)
import Text.Smolder.HTML (div, form, h2, html, input, p) as M
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup (text) as M
import Text.Smolder.Renderer.String as S
import Type.Row (type (+))
import WebRow.Forms (Layout, LayoutBase(..), TextInput) as Forms
import WebRow.Forms.Widgets (TextInputProps(..), _textInput)

str ∷ ∀ msg. Msg msg → String
str { msg } = Lazy.force msg

html ∷ Html Unit → String
html body = S.render $ M.html ! A.lang "en" $ body

type FormLayout widgets msg
  = Forms.Layout (Msg msg) (Forms.TextInput () + widgets)

type RenderWidgets widgets msg
  = VariantF widgets (Msg msg) → Html Unit

formBody ∷ ∀ msg widgets. RenderWidgets widgets msg → FormLayout widgets msg → Html Unit
formBody renderExtra (Forms.Section { closed, errors, layout }) = do
  -- | TODO: Render form errors
  for_ errors \msg → M.p $ M.text (str msg)
  case closed of
    Just { title } → for_ title \t → M.h2 $ M.text (str t)
    Nothing → pure unit
  for_ layout (formBody renderExtra)

formBody renderExtra (Forms.Widget { widget }) =
  M.div
    $ do
        renderWidget widget
  where
  renderWidget =
    renderExtra
      # VariantF.on _textInput \(TextInputProps { name, payload, result, type_ }) → do
          for_ result case _ of
            Just errors →
              for_ errors \msg →
                M.p $ M.text (str msg)
            otherwise → pure unit
          M.input ! A.type' type_ ! A.name name ! A.value (fromMaybe "" (payload >>= Array.head))

form ∷ ∀ msg widgets. RenderWidgets widgets msg → FormLayout widgets msg → Html Unit
form renderExtra l = do
  M.form ! A.method "post"
    $ do
        formBody renderExtra l
        M.input ! A.type' "submit" ! A.value "submit"

-- { dangerouslySetInnerHTML: { __html : "<a href=\"https://google.com\">UNSAFE</a>" }}
form' ∷ ∀ msg. FormLayout () msg → Html Unit
form' = form VariantF.case_

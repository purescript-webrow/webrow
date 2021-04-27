module WebRow.Forms.Widgets where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (inj) as Variant
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Widget (Widget)

_textInput = SProxy ∷ SProxy "textInput"

type TextInputPropsR attrs
  = { label ∷ Maybe String
    , payload ∷ Maybe Payload.Value
    , placeholder ∷ Maybe String
    , helpText ∷ Maybe String
    , name ∷ String
    , result ∷ Maybe (Maybe (Array String))
    , type_ ∷ String
    | attrs
    }

newtype TextInputProps attrs
  = TextInputProps (TextInputPropsR attrs)
derive instance newtypeTextInputProps ∷ Newtype (TextInputProps attrs) _

type TextInput attrs r
  = ( textInput ∷ TextInputProps attrs
    | r
    )

textInput ∷
  ∀ attrs r.
  TextInputPropsR attrs →
  Widget (TextInput attrs + r)
textInput args = Variant.inj _textInput (TextInputProps args)


module WebRow.Forms.Widgets where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (inj) as Variant
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Widget (Widget)

_textInput = SProxy ∷ SProxy "textInput"

type TextInputPropsRow msg attrs
  = ( label ∷ Maybe msg
    , payload ∷ Maybe Payload.Value
    , placeholder ∷ Maybe msg
    , helpText ∷ Maybe msg
    , name ∷ String
    , result ∷ Maybe (Maybe (Array msg))
    , type_ ∷ String
    | attrs
    )

type TextInputPropsR msg attrs = { | TextInputPropsRow msg attrs }

newtype TextInputProps msg attrs
  = TextInputProps (TextInputPropsR msg attrs)
derive instance newtypeTextInputProps ∷ Newtype (TextInputProps msg attrs) _

type TextInput msg attrs r
  = ( textInput ∷ TextInputProps msg attrs
    | r
    )

textInput ∷
  ∀ attrs msg r.
  TextInputPropsR msg attrs →
  Widget (TextInput msg attrs + r)
textInput args = Variant.inj _textInput (TextInputProps args)


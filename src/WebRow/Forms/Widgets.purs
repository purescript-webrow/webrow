module WebRow.Forms.Widgets where

import Prelude

import Data.Functor.Variant (FProxy)
import Data.Functor.Variant (inj) as VariantF
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Widget (Widget)

_textInput = SProxy ∷ SProxy "textInput"

type TextInputPropsRow attrs msg
  = ( label ∷ Maybe msg
    , payload ∷ Maybe Payload.Value
    , placeholder ∷ Maybe msg
    , helpText ∷ Maybe msg
    , name ∷ String
    , result ∷ Maybe (Maybe (Array msg))
    , type_ ∷ String
    | attrs
    )

type TextInputPropsR attrs msg = { | TextInputPropsRow attrs msg }

newtype TextInputProps attrs msg
  = TextInputProps (TextInputPropsR attrs msg)
derive instance newtypeTextInputProps ∷ Newtype (TextInputProps attrs msg) _
derive instance functorTextInputProps ∷ Functor (TextInputProps attrs)

type TextInput attrs r
  = ( textInput ∷ FProxy (TextInputProps attrs)
    | r
    )

textInput ∷
  ∀ attrs msg r.
  TextInputPropsR attrs msg →
  Widget (TextInput attrs + r) msg
textInput args = VariantF.inj _textInput (TextInputProps args)


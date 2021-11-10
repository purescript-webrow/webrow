module WebRow.Forms.Widgets where

import Prelude
import Data.Functor.Variant (inj) as VariantF
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Widget (Widget)

_textInput = Proxy ∷ Proxy "textInput"

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

type TextInputPropsR attrs msg
  = { | TextInputPropsRow attrs msg }

newtype TextInputProps attrs msg
  = TextInputProps (TextInputPropsR attrs msg)

derive instance newtypeTextInputProps ∷ Newtype (TextInputProps attrs msg) _

derive instance functorTextInputProps ∷ Functor (TextInputProps attrs)

instance eqTextInputProps ∷ (Eq { | attrs }, Eq msg) ⇒ Eq (TextInputProps attrs msg) where
  eq (TextInputProps p1) (TextInputProps p2) = do
    let
      contract ∷ { | TextInputPropsRow attrs msg } → { | TextInputPropsRow () msg }
      contract = unsafeCoerce

      rest ∷ { | TextInputPropsRow attrs msg } → { | attrs }
      rest = unsafeCoerce
    (contract p1 == contract p2) && (rest p1 == rest p2)

type TextInput attrs r
  = ( textInput ∷ TextInputProps attrs | r )

textInput ∷
  ∀ attrs msg r.
  TextInputPropsR attrs msg →
  Widget (TextInput attrs + r) msg
textInput args = VariantF.inj _textInput (TextInputProps args)

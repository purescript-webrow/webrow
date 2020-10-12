module WebRow.Forms.Widgets where

import Data.Either (Either)
import Data.Exists (Exists)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Variant (inj) as Variant
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Widget (Widget)

_textInput = SProxy ∷ SProxy "textInput"

type TextInputPropsR
  = { label ∷ Maybe String
    , payload ∷ Maybe Payload.Value
    , placeholder ∷ Maybe String
    , helpText ∷ Maybe String
    , name ∷ String
    , result ∷
        Maybe
          (Either (Array String) (Exists Identity))
    , type_ ∷ String
    }

newtype TextInputProps
  = TextInputProps TextInputPropsR

type TextInput r
  = ( textInput ∷ TextInputProps
    | r
    )

textInput ∷
  ∀ r.
  TextInputPropsR →
  Widget (TextInput + r)
textInput args = Variant.inj _textInput (TextInputProps args)

module WebForm.Forms.Widgets.TextInput where

-- import Prelude
-- 
-- 
-- import Data.Functor.Variant (inj) as Functor.Variant
-- import Data.Maybe (Maybe)
-- import Data.Newtype (class Newtype)
-- import Type.Prelude (SProxy(..))
-- import Type.Row (type (+))
-- import WebRow.Forms.Payload (Value) as Payload
-- import WebRow.Forms.Widget (Widget)
-- 
-- _textInput = SProxy ∷ SProxy "textInput"
-- 
-- type TextInputPropsR onChange
--   = { label ∷ Maybe String
--     , payload ∷ Maybe Payload.Value
--     , onChange ∷ String → onChange
--     , placeholder ∷ Maybe String
--     , helpText ∷ Maybe String
--     , name ∷ String
--     , result ∷ Maybe (Maybe (Array String))
--     , type_ ∷ String
--     }
-- 
-- newtype TextInputProps onChange
--   = TextInputProps (TextInputPropsR onChange)
-- derive instance functorTextInputProps ∷ Functor TextInputProps
-- derive instance newtypeTextInputProps ∷ Newtype (TextInputProps onChange) _
-- 
-- type TextInput r
--   = ( textInput ∷ Proxy TextInputProps
--     | r
--     )
-- 
-- textInput ∷
--   ∀ onChange r.
--   TextInputPropsR onChange →
--   Widget onChange (TextInput + r)
-- textInput args = Functor.Variant.inj _textInput (TextInputProps args)
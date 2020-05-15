module WebRow.Forms.Fields where

import Prelude

import Data.Either (Either(..))
import Data.Exists (Exists)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (inj) as Variant
import Record (insert) as Record
import Type.Prelude (SProxy(..))
import WebRow.Forms.Layout (Layout(..)) as Layout
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Sequence (id) as Sequence


type ValidationCtx r msg o = { input ∷ Maybe Payload.Value, result ∷ Maybe (Either (Array msg) o) | r }

type Base r msg o = ValidationCtx (name ∷ String | r) msg o

type Initials msg o = Base () msg o

-- | Basic text input which ignores the partial validation result
type TextInputBase r msg result = Base (type_ ∷ String | r) msg result

-- | An "untyped" TextInput which ignores correctly parsed value during rendering
type TextInput msg = TextInputBase () msg (Exists Identity)

type FieldRow msg extra =
  ( "textInput" ∷ TextInput msg
  -- , "numberInput" ∷ { name ∷ String, input ∷ Maybe (Array String), result ∷ Maybe (Either msg Number) }
  | extra
  )

_name = SProxy ∷ SProxy "name"

_textInput = SProxy ∷ SProxy "textInput"

_type = SProxy ∷ SProxy "type_"


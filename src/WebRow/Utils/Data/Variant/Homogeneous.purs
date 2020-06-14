module WebRow.Utils.Data.Variant.Homogeneous where

import Prelude

import Data.Array (elemIndex, fromFoldable) as Array
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record.Extra (class Keys, keys) as Record.Extra
import Record.Unsafe (unsafeGet) as Record.Unsafe
import Type.Eval (class Eval)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Row (RProxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

type MapConst a = ToRow <<< Map (Const a) <<< FromRow

-- | To be honest this doesn't require homogeneous variant as input ;-)
recordGet
  ∷ ∀ a missing r v v'
  . Eval (MapConst a (RProxy v)) (RProxy v')
  ⇒ Union v' missing r
  ⇒ Homogeneous r a
  ⇒ Variant v
  → Record r
  → a
recordGet v | VariantRep r ← unsafeCoerce v = Record.Unsafe.unsafeGet r.type r.value

get ∷ ∀ a row. Variant row → a
get v | VariantRep r ← unsafeCoerce v = r.value

set ∷ ∀ a row. Homogeneous row a ⇒ a → Variant row → Variant row
set a v | VariantRep r ← unsafeCoerce v = unsafeCoerce (VariantRep { "type": r.type, value: a })

parse ∷ ∀ a rl row. RowToList row rl ⇒ Record.Extra.Keys rl ⇒ Homogeneous row a ⇒ a → String → Maybe (Variant row)
parse value =
  let
    labels = Array.fromFoldable (Record.Extra.keys (RProxy ∷ RProxy row))
  in
    \label → case Array.elemIndex label labels of
      Just _ →  Just (unsafeCoerce (VariantRep { type: label, value }))
      Nothing → Nothing

_variant ∷ ∀ a row. Homogeneous row a ⇒ Lens' (Variant row) a
_variant = lens get (flip set)


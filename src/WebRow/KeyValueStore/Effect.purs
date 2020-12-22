module WebRow.KeyValueStore.Effect where

import Prelude
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Type.Prelude (class IsSymbol)
import Type.Row (type (+))

type Key
  = String

-- | TODO: We should probably add `attrs` parameter
-- | into the mix so we can handle things like TTL etc.
data KeyValueStoreF val a
  = DeleteF Key a
  | GetF Key (Maybe val → a)
  | PutF Key val (Boolean → a)
  | NewF (Key → a)

derive instance functorDataStoreF ∷ Functor (KeyValueStoreF val)

type KEYVALUESTORE v
  = FProxy (KeyValueStoreF v)

type KeyValueStore v eff
  = ( keyValueStore ∷ KEYVALUESTORE v | eff )

liftKeyValueStoreAt ∷
  ∀ a eff eff_ s v.
  IsSymbol s ⇒
  Row.Cons s (KEYVALUESTORE v) eff_ eff ⇒
  SProxy s →
  KeyValueStoreF v a →
  Run eff a
liftKeyValueStoreAt = Run.lift

_keyValueStore = SProxy ∷ SProxy "keyValueStore"

deleteAt ∷
  ∀ eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (KEYVALUESTORE v) eff_ eff ⇒
  (SProxy l) →
  Key →
  Run eff Unit
deleteAt l key = liftKeyValueStoreAt l (DeleteF key unit)

delete ∷
  ∀ eff v.
  Key →
  Run (KeyValueStore v + eff) Unit
delete = deleteAt _keyValueStore

getAt ∷
  ∀ eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (KEYVALUESTORE v) eff_ eff ⇒
  SProxy l →
  Key →
  Run eff (Maybe v)
getAt l key = liftKeyValueStoreAt l (GetF key identity)

get ∷
  ∀ eff v.
  Key →
  Run (KeyValueStore v + eff) (Maybe v)
get = getAt _keyValueStore

putAt ∷
  ∀ eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (KEYVALUESTORE v) eff_ eff ⇒
  SProxy l →
  Key →
  v →
  Run eff Boolean
putAt l key val = liftKeyValueStoreAt l (PutF key val identity)

put ∷
  ∀ eff v.
  Key →
  v →
  Run (KeyValueStore v + eff) Boolean
put = putAt _keyValueStore

newAt ∷
  ∀ eff eff_ l v.
  IsSymbol l ⇒
  Row.Cons l (KEYVALUESTORE v) eff_ eff ⇒
  SProxy l →
  Run eff Key
newAt l = liftKeyValueStoreAt l (NewF identity)

new ∷
  ∀ eff v.
  Run (KeyValueStore v + eff) Key
new = newAt _keyValueStore

module WebRow.KeyValueStore.Interpret where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify) as Argonaut
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy)
import Data.UUID (genUUID)
import Data.UUID (toString) as UUID
import Effect (Effect)
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Type.Prelude (class IsSymbol)
import Type.Row (type (+))
import WebRow.KeyValueStore.Effect (KEYVALUESTORE, Key, KeyValueStore, KeyValueStoreF(..), _keyValueStore)

type Namespace
  = String

newKey ∷ Namespace → Effect Key
newKey namespace = (append namespace <<< UUID.toString) <$> genUUID

-- | Do we want to drop `delete` and use `put ∷ Key → Maybe a → m Boolean`
type Interface m a
  = { delete ∷ Key → m Boolean
    , get ∷ Key → m (Maybe a)
    , put ∷ Key → a → m Boolean
    , new ∷ m Key
    }

hoist ∷ ∀ m m'. (m ~> m') → Interface m ~> Interface m'
hoist h s = { delete, get, new, put }
  where
  new = h s.new

  delete = h <$> s.delete

  get = h <$> s.get

  put k = h <$> s.put k


imapInterface ∷
  ∀ a b m.
  Monad m ⇒
  { parse ∷ a → Maybe b
  , print ∷ b → a
  } →
  Interface m a →
  Interface m b
imapInterface { parse, print } kv =
  { delete: kv.delete
  , get: kv.get >=> (_ >>= parse) >>> pure
  , new: kv.new
  , put: \k v → kv.put k (print v)
  }

jsonify ∷
  ∀ m.
  Monad m ⇒
  Interface m String →
  Interface m Json
jsonify =
  imapInterface
    { parse: Argonaut.jsonParser >>> hush
    , print: Argonaut.stringify
    }

argonautify ∷
  ∀ a m.
  Monad m ⇒
  Argonaut.DecodeJson a ⇒
  Argonaut.EncodeJson a ⇒
  Interface m String →
  Interface m a
argonautify =
  jsonify
    >>> imapInterface
        { parse: Argonaut.decodeJson >>> hush
        , print: Argonaut.encodeJson
        }

runOnInterfaceAt ∷
  ∀ a eff eff' l v.
  IsSymbol l ⇒
  Row.Cons l (KEYVALUESTORE v) eff eff' ⇒
  SProxy l →
  Interface (Run eff) v →
  Run eff' a → Run eff a
runOnInterfaceAt l interface = Run.interpret (Run.on l handleKeyValueStore Run.send)
  where
  handleKeyValueStore ∷ ∀ b. KeyValueStoreF v b → Run eff b
  handleKeyValueStore = case _ of
    DeleteF k next → pure next
    NewF next → interface.new <#> next
    GetF k next → interface.get k <#> next
    PutF k v next → interface.put k v <#> next

runOnInterface ∷
  ∀ a eff v.
  Interface (Run eff) v →
  Run (KeyValueStore v + eff) a →
  Run eff a
runOnInterface = runOnInterfaceAt _keyValueStore

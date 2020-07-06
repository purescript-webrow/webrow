module WebRow.KeyValueStore.Types where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify) as Argonaut
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.UUID (genUUID)
import Data.UUID (toString) as UUID
import Effect (Effect)
import Prim.Row (class Cons, class Union) as Row
import Run (Run)
import Run (expand) as Run
import Run.Reader (READER, askAt)
import Type.Prelude (class IsSymbol, SProxy)

type Key = String
type Namespace = String

type KeyValueStore m a =
  { delete ∷ Key → m Boolean
  , get ∷ Key → m (Maybe a)
  , put ∷ Key → a → m Boolean
  , new ∷ m Key
  }

newKey ∷ Namespace → Effect Key
newKey namespace = (append namespace <<< UUID.toString) <$> genUUID

hoist ∷ ∀ a m m'. (m ~> m') → KeyValueStore m a → KeyValueStore m' a
hoist h s = { delete, get, new, put }
  where
  new = h s.new
  delete = h <$> s.delete
  get = h <$> s.get
  put k = h <$> s.put k

-- | We want to avoid infinite recurssion on the type
-- | level so we need something like:
-- |
-- | ```purescript
-- | Row.Union sEff t1 eff
-- | ```
type KEYVALUESTORE sEff a = READER (KeyValueStore (Run sEff) a)

keyValueStoreAt ::
  ∀ a eff l s t1 t2
  . IsSymbol l
  ⇒ Row.Union s t1 eff
  ⇒ Row.Cons l (KEYVALUESTORE s a) t2 eff
  ⇒ SProxy l
  → Run eff (KeyValueStore (Run eff) a)
keyValueStoreAt l = askAt l >>= hoist Run.expand >>> pure

imapKeyValueStore
  ∷ ∀ a b m
  . Monad m
  ⇒ { parse ∷ a → Maybe b
    , print ∷ b → a
    }
  → KeyValueStore m a
  → KeyValueStore m b
imapKeyValueStore { parse, print } kv =
  { delete: kv.delete
  , get: kv.get >=> (_ >>= parse) >>> pure
  , new: kv.new
  , put: \k v → kv.put k (print v)
  }

jsonify
  ∷ ∀ m
  . Monad m
  ⇒ KeyValueStore m String
  → KeyValueStore m Json
jsonify = imapKeyValueStore
  { parse: Argonaut.jsonParser >>> hush
  , print: Argonaut.stringify
  }

argounatify
  ∷ ∀ a m
  . Monad m
  ⇒ Argonaut.DecodeJson a
  ⇒ Argonaut.EncodeJson a
  ⇒ KeyValueStore m String
  → KeyValueStore m a
argounatify = jsonify >>> imapKeyValueStore
  { parse: Argonaut.decodeJson >>> hush
  , print: Argonaut.encodeJson
  }


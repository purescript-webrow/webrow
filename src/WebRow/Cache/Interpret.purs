module WebRow.Cache.Interpret where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify) as Argonaut
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy)
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Type.Prelude (class IsSymbol)
import Type.Row (type (+))
import WebRow.Cache.Effect (CACHE, CacheF(..), Key, Cache, _cache)

-- | Do we want to drop `delete` and use `put ∷ Key → Maybe a → m Boolean`
type Interface m attrs a
  = { delete ∷ Key → m Boolean
    , lookup ∷ Key → m (Maybe a)
    , insert ∷ Key → attrs → a → m Boolean
    }

hoist ∷ ∀ attrs m m'. (m ~> m') → Interface m attrs ~> Interface m' attrs
hoist h s = { delete, insert, lookup }
  where
  delete = h <$> s.delete

  lookup = h <$> s.lookup

  insert k attrs = h <$> s.insert k attrs


imapInterface ∷
  ∀ a attrs b m.
  Monad m ⇒
  { parse ∷ a → Maybe b
  , print ∷ b → a
  } →
  Interface m attrs a →
  Interface m attrs b
imapInterface { parse, print } c =
  { delete: c.delete
  , lookup: c.lookup >=> (_ >>= parse) >>> pure
  , insert: \k attrs v → c.insert k attrs (print v)
  }

jsonify ∷
  ∀ attrs m.
  Monad m ⇒
  Interface m attrs String →
  Interface m attrs Json
jsonify =
  imapInterface
    { parse: Argonaut.jsonParser >>> hush
    , print: Argonaut.stringify
    }

argonautify ∷
  ∀ a attrs m.
  Monad m ⇒
  Argonaut.DecodeJson a ⇒
  Argonaut.EncodeJson a ⇒
  Interface m attrs String →
  Interface m attrs a
argonautify =
  jsonify
    >>> imapInterface
        { parse: Argonaut.decodeJson >>> hush
        , print: Argonaut.encodeJson
        }

runOnInterfaceAt ∷
  ∀ a attrs eff eff' l v.
  IsSymbol l ⇒
  Row.Cons l (CACHE attrs v) eff eff' ⇒
  SProxy l →
  Interface (Run eff) attrs v →
  Run eff' a → Run eff a
runOnInterfaceAt l interface = Run.interpret (Run.on l handleKeyValueStore Run.send)
  where
  handleKeyValueStore ∷ ∀ b. CacheF attrs v b → Run eff b
  handleKeyValueStore = case _ of
    DeleteF k next → pure next
    LookupF k next → interface.lookup k <#> next
    InsertF k v attrs next → interface.insert k v attrs <#> next

runOnInterface ∷
  ∀ a attrs eff v.
  Interface (Run eff) attrs v →
  Run (Cache attrs v + eff) a →
  Run eff a
runOnInterface = runOnInterfaceAt _cache

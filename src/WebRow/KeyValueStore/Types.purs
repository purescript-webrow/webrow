module WebRow.KeyValueStore.Types where

import Prelude

import Data.Maybe (Maybe)
import Prim.Row (class Cons, class Union) as Row
import Run (Run)
import Run (expand) as Run
import Run.Reader (READER, askAt)
import Type.Prelude (class IsSymbol, SProxy)

type Key = String

type KeyValueStore m a =
  { create ∷ m Key
  , delete ∷ Key → m Unit
  , get ∷ Key → m (Maybe a)
  , set ∷ Key → a → m Unit
  }

hoist ∷ ∀ a m m'. (m ~> m') → KeyValueStore m a → KeyValueStore m' a
hoist h s = { create, delete, get, set }
  where
  create = h s.create
  delete = h <$> s.delete
  get = h <$> s.get
  set k = h <$> s.set k

type KeyValueRun eff a = KeyValueStore (Run eff) a

keyValueStoreAt ∷
  ∀ a eff l s t1 t2
  . IsSymbol l
  ⇒ Row.Union s t1 eff
  ⇒ Row.Cons l (READER (KeyValueRun s a)) t2 eff
  ⇒ SProxy l
  → Run eff (KeyValueRun eff a)
keyValueStoreAt l = askAt l >>= hoist Run.expand >>> pure


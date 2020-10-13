module WebRow.Session.SessionStore where

import Prelude

import Data.Lazy (Lazy, defer, force)
import Data.Maybe (fromMaybe)
import Prim.Row (class Union) as Row
import Run (Run)
import Run (expand) as Run
import WebRow.KeyValueStore (KeyValueStore, Key)

type SessionStore m session
  = { delete ∷ m Boolean
    , fetch ∷ m session
    , key ∷ Lazy Key
    , save ∷ session → m Boolean
    }

hoist ∷ ∀ a m m'. (m ~> m') → SessionStore m a → SessionStore m' a
hoist h s =
  { delete: h s.delete
  , fetch: h s.fetch
  , key: s.key
  , save: h <$> s.save
  }

expand ∷
  ∀ a eff sEff sEff_.
  Row.Union sEff sEff_ eff ⇒
  SessionStore (Run sEff) a →
  SessionStore (Run eff) a
expand = hoist Run.expand

new ∷
  ∀ m session.
  Monad m ⇒
  session →
  KeyValueStore m session →
  m (SessionStore m session)
new default kv =
  kv.new
    >>= \k →
        pure
          { delete: kv.delete k
          , fetch: kv.get k >>= fromMaybe default >>> pure
          , key: defer \_ → k
          , save: kv.put k
          }

forKey ∷
  ∀ m session.
  Monad m ⇒
  session →
  Lazy Key →
  KeyValueStore m session →
  SessionStore m session
forKey default k kv =
  { delete: kv.delete (force k)
  , fetch: kv.get (force k) >>= fromMaybe default >>> pure
  , key: k
  , save: kv.put (force k)
  }

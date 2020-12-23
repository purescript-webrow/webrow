module WebRow.Session.SessionStore where

import Prelude

import Data.Lazy (Lazy, force)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Seconds)
import Prim.Row (class Union) as Row
import Run (Run)
import Run (expand) as Run
import WebRow.Cache (Interface, Key) as Cache

newtype TTL = TTL Seconds

type SessionStore m session
  = { delete ∷ m Boolean
    , fetch ∷ m session
    , key ∷ Lazy Cache.Key
    , save ∷ TTL → session → m Boolean
    }

hoist ∷ ∀ a m m'. (m ~> m') → SessionStore m a → SessionStore m' a
hoist h s =
  { delete: h s.delete
  , fetch: h s.fetch
  , key: s.key
  , save: \ttl → h <$> s.save ttl
  }

expand ∷
  ∀ a eff sEff sEff_.
  Row.Union sEff sEff_ eff ⇒
  SessionStore (Run sEff) a →
  SessionStore (Run eff) a
expand = hoist Run.expand

-- new ∷
--   ∀ attrs m session.
--   Monad m ⇒
--   session →
--   Cache.Interface m { ttl ∷ Milliseconds | attrs } session →
--   m (SessionStore m session)
-- new default kv =
--   kv.new
--     >>= \k →
--         pure
--           { delete: kv.delete k
--           , fetch: kv.get k >>= fromMaybe default >>> pure
--           , key: defer \_ → k
--           , save: kv.put k
--           }

forKey ∷
  ∀ m session.
  Monad m ⇒
  session →
  Lazy Cache.Key →
  Cache.Interface m TTL session →
  SessionStore m session
forKey default k kv =
  { delete: kv.delete (force k)
  , fetch: kv.lookup (force k) >>= fromMaybe default >>> pure
  , key: k
  , save: \ttl v → kv.insert (force k) ttl v
  }

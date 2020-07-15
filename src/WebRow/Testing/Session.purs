module WebRow.Testing.Session where

import Prelude

import Data.Lazy (Lazy, defer)
import Data.Lazy (force) as Lazy
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import Run (Run)
import Run (interpret, liftEffect, on, send) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.Forms.Payload (Key)
import WebRow.Session (Session, SessionF(..), _session)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (lazy) as SessionStore.InMemory

type SessionStoreConfig session =
  { default ∷ session
  , key ∷ Maybe Key
  , ref ∷ Ref (Map String session)
  }

handleSession
  ∷ ∀ eff session
  . Lazy (SessionStore (Run eff) session) → SessionF session ~> Run eff
handleSession ss (DeleteF next) = (Lazy.force ss).delete >>= next >>> pure
handleSession ss (FetchF next) = (Lazy.force ss).fetch >>= next >>> pure
handleSession ss (SaveF v next) = (Lazy.force ss).save v >>= next >>> pure

run
  ∷ ∀ eff session
  . Lazy (SessionStore (Run eff) session)
  → Run (Session session + eff)
  ~> Run (eff)
run ss action = Run.interpret (Run.on _session (handleSession ss) Run.send) action

runInMemory ∷ ∀ a eff session
  . SessionStoreConfig session
  → Run (EffRow + Session session + eff) a
  → Run (EffRow + eff) a
runInMemory { default, key, ref } action = do
  lazySessionStore ← Run.liftEffect $
    SessionStore.InMemory.lazy ref default (defer \_ → key)
  let
    ss = map (SessionStore.hoist Run.liftEffect) lazySessionStore
  run ss action


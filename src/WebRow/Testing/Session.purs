module WebRow.Testing.Session where

import Prelude

import Data.Argonaut (Json)
import Data.Lazy (defer)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref (Ref)
import Polyform.Validator.Dual.Pure (Dual) as Pure
import Run (Run)
import Run (interpret, liftEffect, on, send) as Run
import Type.Row (type (+))
import WebRow.Contrib.Run (EffRow)
import WebRow.Forms.Payload (Key)
import WebRow.Session (Session, SessionF(..), _session)
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (hoist) as SessionStore
import WebRow.Session.SessionStore.InMemory (new) as SessionStore.InMemory

type SessionStoreConfig session
  = { default ∷ session
    , key ∷ Maybe Key
    , ref ∷ Ref (Map String session)
    }

handleSession ∷
  ∀ eff session.
  Effect (SessionStore (Run (EffRow + eff)) session) → SessionF session ~> Run (EffRow + eff)
handleSession ss (DeleteF next) = Run.liftEffect ss >>= _.delete >>= next >>> pure

handleSession ss (FetchF next) = Run.liftEffect ss >>= _.fetch >>= next >>> pure

handleSession ss (SaveF v next) = do
  ss' ← Run.liftEffect ss
  ss'.save v >>= next >>> pure

run ∷
  ∀ eff session.
  Effect (SessionStore (Run (EffRow + eff)) session) →
  Run (EffRow + Session session + eff)
    ~> Run (EffRow + eff)
run ss action = Run.interpret (Run.on _session (handleSession ss) Run.send) action

runInMemory ∷
  ∀ a eff session.
  SessionStoreConfig session →
  Run (EffRow + Session session + eff) a →
  Run (EffRow + eff) a
runInMemory { default, key, ref } action = do
  let
    ss = SessionStore.InMemory.new ref default (defer \_ → key)

    ss' = map (SessionStore.hoist Run.liftEffect) $ ss
  run ss' action

type SessionCookieConfig session
  = { default ∷ session
    , dual ∷ ∀ err. Pure.Dual err Json session
    }

module WebRow.Session where

import Prelude

import HTTPure (empty) as Headers
import Prim.Row (class Union) as Row
import Run (FProxy, Run, SProxy(..))
import Run (interpret, lift, on, send) as Run
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, internalServerError)
import WebRow.Session.SessionStore.Run (SessionStoreRow)
import WebRow.Session.SessionStore.Run (delete, fetch, save) as SessionStore.Run

-- | TODO: Move these pieces to Session.Effects
data SessionF session a
  = DeleteF (Boolean → a)
  | FetchF (session → a)
  | SaveF session (Boolean → a)

derive instance functorSessionF ∷ Functor (SessionF session)

type SESSION session = FProxy (SessionF session)

_session = SProxy ∷ SProxy "session"

type Session session r = (session ∷ SESSION session | r)

delete
  ∷ ∀ eff session
  . Run (Session session + eff) Boolean
delete = Run.lift _session (DeleteF identity)

modify
  ∷ ∀ eff session
  . (session → session) → Run (HTTPExcept + Session session + eff) Unit
modify f = fetch >>= f >>> save

fetch
  ∷ ∀ eff session
  . Run (Session session + eff) session
fetch = Run.lift _session (FetchF identity)

save
  ∷ ∀ eff session
  . session → Run (HTTPExcept + Session session + eff) Unit
save session = Run.lift _session (SaveF session identity) >>= not >>> if _
  then
    -- | Collect more request info etc.
    internalServerError Headers.empty "Serious problem on our side..."
  else
    pure unit

-- | To use clean API we provide effect layer
handleSession
  ∷ ∀ eff sEff sEff_ session
  . Row.Union sEff sEff_ (SessionStoreRow sEff session + eff)
  ⇒ SessionF session ~> Run (SessionStoreRow sEff session + eff)
handleSession (DeleteF next) = SessionStore.Run.delete >>= next >>> pure
handleSession (FetchF next) = SessionStore.Run.fetch >>= next >>> pure
handleSession (SaveF v next) = SessionStore.Run.save v >>= next >>> pure

runSession ∷ ∀ eff sEff sEff_ session
  .  Row.Union sEff sEff_ ( SessionStoreRow sEff session + eff)
  ⇒ Run ( Session session + SessionStoreRow sEff session + eff)
  ~> Run ( SessionStoreRow sEff session + eff)
runSession = Run.interpret (Run.on _session handleSession Run.send)

module WebRow.Session.SessionStore.Run where

import Prelude

import Prim.Row (class Union) as Row
import Run (Run, SProxy(..))
import Run.Reader (READER, askAt)
import Type.Row (type (+))
import WebRow.Session.SessionStore (SessionStore)
import WebRow.Session.SessionStore (expand) as SessionStore

_sessionStore = SProxy ∷ SProxy "sessionStore"

type SessionStoreRow eff session r =
  (sessionStore ∷ READER (SessionStore (Run eff) session) | r)

sessionStore
  ∷ ∀ eff sEff sEff_ session
  . Row.Union sEff sEff_ (SessionStoreRow sEff session + eff)
  ⇒ Run
    (SessionStoreRow sEff session + eff)
    (SessionStore (Run (SessionStoreRow sEff session + eff)) session)
sessionStore = askAt _sessionStore <#> SessionStore.expand

delete
  ∷ ∀ eff sEff sEff_ session
  . Row.Union sEff sEff_ (SessionStoreRow sEff session + eff)
  ⇒ Run (SessionStoreRow sEff session + eff) Boolean
delete = sessionStore >>= _.delete

fetch
  ∷ ∀ eff sEff sEff_ session
  . Row.Union sEff sEff_ (SessionStoreRow sEff session + eff)
  ⇒ Run (SessionStoreRow sEff session + eff) session
fetch = sessionStore >>= _.fetch

save
  ∷ ∀ eff sEff sEff_ session
  . Row.Union sEff sEff_ (SessionStoreRow sEff session + eff)
  ⇒ session
  → Run (SessionStoreRow sEff session + eff) Boolean
save session = sessionStore >>= (\s → s.save session)


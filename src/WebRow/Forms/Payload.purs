module WebRow.Forms.Payload where

import Prelude
import Data.Lazy (force) as L
import Data.Maybe (Maybe, maybe)
import Polyform.Batteries.UrlEncoded (Query) as UrlEncoded
import Polyform.Batteries.UrlEncoded.Query (Key, lookup) as UrlDecoded.Query
import Polyform.Batteries.UrlEncoded.Query (parse, Value, Query) as UrlEncoded.Query
import Run (Run)
import Run.Reader (askAt)
import Type.Row (type (+))
import WebRow.HTTP (HTTPExcept, Request, badRequest'', body)
import WebRow.Routing (Routing, _routing)

type Value
  = UrlEncoded.Query.Value

type Key
  = UrlDecoded.Query.Key

-- | Map String (Array String)
type UrlDecoded
  = UrlEncoded.Query

lookup ∷ String → UrlDecoded → Maybe (Array String)
lookup = UrlDecoded.Query.lookup

fromQuery ∷ ∀ eff route. Run (Routing route + eff) UrlDecoded
fromQuery =
  pure
    <<< L.force
    <<< _.query
    =<< askAt _routing

fromBody ∷ ∀ eff. Run (Request + HTTPExcept + eff) UrlDecoded
fromBody = do
  bodyStr ← body
  maybe badRequest'' pure (UrlEncoded.Query.parse { replacePlus: true } bodyStr)

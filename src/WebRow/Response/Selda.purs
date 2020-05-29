module WebRow.Response.Selda where

import Prelude

import Data.Maybe (maybe)
import HTTPure as Httpure
import Run (Run)
import Selda (Query, Table, selectFrom)
import Selda.PG.Class (BackendPGClass, query1)
import Selda.Query (class FromTable)
import Selda.Query.Class (class GenericQuery)
import SeldaUtils.Class (SeldaPG)
import SeldaUtils.Effect (SELDA, liftSelda)
import WebRow.Response (RESPONSE)
import WebRow.Response as Response

getObjectOr404
  ∷ ∀ t cols i o res eff
  . FromTable Unit t cols
  ⇒ GenericQuery BackendPGClass SeldaPG i o
  ⇒ Table t
  → ({ | cols } → Query Unit { | i })
  → Run
      ( selda ∷ SELDA
      , response ∷ RESPONSE res
      | eff
      )
      { | o }
getObjectOr404 table k = liftSelda (query1 q) >>= maybe notFound pure
  where
    notFound = Response.notFound Httpure.empty
    q = selectFrom table k

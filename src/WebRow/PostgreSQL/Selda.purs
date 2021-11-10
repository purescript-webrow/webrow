module WebRow.PostgreSQL.Selda where

import Prelude
import Data.Maybe (Maybe)
import Database.PostgreSQL (class FromSQLRow)
import Run (Run)
import Selda (Col, FullQuery, Table)
import Selda.Col (class GetCols)
import Selda.PG.Aff (PGSelda)
import Selda.PG.Aff (deleteFrom, insert, insert1, insert1_, insert_, query, query1, update) as Selda.PG.Aff
import Selda.PG.Class (class InsertRecordIntoTableReturning, BackendPGClass)
import Selda.Query.Class (class GenericInsert)
import Selda.Query.Utils (class ColsToPGHandler, class TableToColsWithoutAlias)
import Type.Row (type (+))
import WebRow.PostgreSQL.Internal (connection, liftPgAff)
import WebRow.PostgreSQL.PG (PG)

query ∷
  ∀ eff i mode o tup.
  ColsToPGHandler BackendPGClass i tup o ⇒
  GetCols i ⇒
  FromSQLRow tup ⇒
  FullQuery BackendPGClass { | i } →
  Run (PG mode + eff) (Array { | o })
query q = do
  conn ← connection
  liftPgAff (Selda.PG.Aff.query conn q)

query1 ∷
  ∀ eff i o mode tup.
  ColsToPGHandler BackendPGClass i tup o ⇒
  GetCols i ⇒
  FromSQLRow tup ⇒
  FullQuery BackendPGClass { | i } →
  Run (PG mode + eff) (Maybe { | o })
query1 q = do
  conn ← connection
  liftPgAff (Selda.PG.Aff.query1 conn q)

insert ∷
  ∀ eff mode r t ret.
  InsertRecordIntoTableReturning r t ret ⇒
  Table t → Array { | r } → Run (PG mode + eff) (Array { | ret })
insert table xs = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.insert conn table xs

insert_ ∷
  ∀ eff mode r t.
  GenericInsert BackendPGClass PGSelda t r ⇒
  Table t → Array { | r } → Run (PG mode + eff) Unit
insert_ table xs = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.insert_ conn table xs

insert1 ∷
  ∀ eff mode r t ret.
  InsertRecordIntoTableReturning r t ret ⇒
  Table t → { | r } → Run (PG mode + eff) { | ret }
insert1 table xs = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.insert1 conn table xs

insert1_ ∷
  ∀ eff mode t r.
  GenericInsert BackendPGClass PGSelda t r ⇒
  Table t → { | r } → Run (PG mode + eff) Unit
insert1_ table r = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.insert1_ conn table r

deleteFrom ∷
  ∀ eff mode r r'.
  TableToColsWithoutAlias BackendPGClass r r' ⇒
  Table r →
  ({ | r' } → Col BackendPGClass Boolean) →
  Run (PG mode + eff) Unit
deleteFrom table r = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.deleteFrom conn table r

update ∷
  ∀ eff mode r r'.
  TableToColsWithoutAlias BackendPGClass r r' ⇒
  GetCols r' ⇒
  Table r →
  ({ | r' } → Col BackendPGClass Boolean) →
  ({ | r' } → { | r' }) →
  Run (PG mode + eff) Unit
update table pred up = do
  conn ← connection
  liftPgAff $ Selda.PG.Aff.update conn table pred up

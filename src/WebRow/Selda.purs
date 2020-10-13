module WebRow.Selda where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Database.PostgreSQL (Connection, PGError, Query(..), execute) as PG
import Database.PostgreSQL (PGError, Row0(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, Query, query) as PostgreSQL
import Effect.Aff (Aff)
import Run (Run)
import Run as Run
import Run.Except (EXCEPT, throwAt)
import Run.Reader (READER, askAt)
import Selda (Col, FullQuery, Table)
import Selda.PG.Class (class InsertRecordIntoTableReturning, BackendPGClass)
import Selda.PG.Class (deleteFrom, insert, insert1, insert1_, query, query1, update) as PG.Class
import Selda.Query.Class (class GenericDelete, class GenericInsert, class GenericQuery, class GenericUpdate)
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow)

type SeldaPG
  = ExceptT PG.PGError (ReaderT PG.Connection Aff)

-- | I'm not sure if this represtation is consistent.
-- | I want to allow some inspection but also compile
-- | those actions without coercing.
-- |
-- | * Probably we could split query and keep `FullQuery`.
-- | * We can add `∀ i o. Query i o` to `PgQueryF`.
data SeldaF a
  = SeldaF (SeldaPG a)
  | PgExecuteF String (Unit → a)
  | PgQueryF (PG.Connection → Aff (Either PGError a))

derive instance functorSeldaF ∷ Functor SeldaF

type SELDA
  = FProxy SeldaF

type Selda eff
  = ( selda ∷ SELDA | eff )

_selda = SProxy ∷ SProxy "selda"

type PgError r
  = ( pgError ∷ EXCEPT PGError | r )

type PgConnection r
  = ( pgConnection ∷ READER { conn ∷ PG.Connection, inTransaction ∷ Boolean } | r )

_pgConnection = SProxy ∷ SProxy "pgConnection"

_pgError = SProxy ∷ SProxy "pgError"

query ∷
  ∀ eff o i.
  GenericQuery BackendPGClass SeldaPG i o ⇒
  FullQuery BackendPGClass { | i } →
  Run (Selda + eff) (Array { | o })
query q = do
  Run.lift _selda (SeldaF (PG.Class.query q))

query1 ∷
  ∀ eff o i.
  GenericQuery BackendPGClass SeldaPG i o ⇒
  FullQuery BackendPGClass { | i } → Run (Selda + eff) { | o }
query1 q = Run.lift _selda (SeldaF (PG.Class.query1 q))

insert ∷
  ∀ eff r t ret.
  InsertRecordIntoTableReturning r t ret ⇒
  Table t → Array { | r } → Run (Selda + eff) (Array { | ret })
insert table xs = do
  Run.lift _selda (SeldaF (PG.Class.insert table xs))

insert1_ ∷
  ∀ eff t r.
  GenericInsert BackendPGClass SeldaPG t r ⇒
  Table t → { | r } → Run (Selda + eff) Unit
insert1_ table r = Run.lift _selda (SeldaF (PG.Class.insert1_ table r))

insert1 ∷
  ∀ eff r t ret.
  InsertRecordIntoTableReturning r t ret ⇒
  Table t → { | r } → Run (Selda + eff) { | ret }
insert1 table r = Run.lift _selda (SeldaF (PG.Class.insert1 table r))

deleteFrom ∷
  ∀ eff t r.
  GenericDelete BackendPGClass SeldaPG t r ⇒
  Table t → ({ | r } → Col BackendPGClass Boolean) → Run (Selda + eff) Unit
deleteFrom table r = Run.lift _selda (SeldaF (PG.Class.deleteFrom table r))

update ∷
  ∀ eff t r.
  GenericUpdate BackendPGClass SeldaPG t r ⇒
  Table t → ({ | r } → Col BackendPGClass Boolean) → ({ | r } → { | r }) → Run (Selda + eff) Unit
update table cond set = Run.lift _selda (SeldaF (PG.Class.update table cond set))

pgExecute ∷ ∀ eff. String → Run (Selda + eff) Unit
pgExecute q = Run.lift _selda (PgExecuteF q identity)

pgQuery ∷
  ∀ eff i o.
  PostgreSQL.ToSQLRow i ⇒
  PostgreSQL.FromSQLRow o ⇒
  PostgreSQL.Query i o →
  i →
  Run (Selda + eff) (Array o)
pgQuery q i = Run.lift _selda (PgQueryF (\conn → PostgreSQL.query conn q i))

run ∷
  ∀ eff.
  Run (AffRow + PgConnection + PgError + Selda + eff)
    ~> Run (AffRow + PgConnection + PgError + eff)
run = Run.run (Run.on _selda handleSelda Run.send)
  where
  handleSelda ∷ ∀ a. SeldaF a → Run (AffRow + PgConnection + PgError + eff) a
  handleSelda action = do
    { conn, inTransaction } ← askAt _pgConnection
    case action of
      SeldaF q → do
        Run.liftAff (runReaderT (runExceptT q) conn)
          >>= case _ of
              Right next → pure next
              Left err → throwAt _pgError err
      PgExecuteF q next → do
        (Run.liftAff $ PG.execute conn (PG.Query q) Row0)
          >>= case _ of
              Just err → throwAt _pgError err
              Nothing → pure (next unit)
      PgQueryF q → do
        (Run.liftAff (q conn))
          >>= case _ of
              Left err → throwAt _pgError err
              Right next → pure next

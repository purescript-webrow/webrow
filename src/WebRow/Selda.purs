module WebRow.Selda where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Database.PostgreSQL (PGError, Row0(..))
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, Connection, PGError, Pool, Query(..), ConnectResult, connect, execute, query) as PG
import Effect.Aff (Aff)
import Run (Run)
import Run as Run
import Run.Except (EXCEPT, throwAt)
import Run.State (STATE, getAt, modifyAt, putAt)
import Selda (Col, FullQuery, Table)
import Selda.PG.Class (class InsertRecordIntoTableReturning, BackendPGClass)
import Selda.PG.Class (deleteFrom, insert, insert1, insert1_, query, query1, update) as PG.Class
import Selda.Query.Class (class GenericDelete, class GenericInsert, class GenericQuery, class GenericUpdate)
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow, EffRow)

type SeldaPG
  = ExceptT PG.PGError (ReaderT PG.Connection Aff)

-- | I'm not sure if this represtation is consistent.
-- | I want to allow some intronspection but also compile
-- | those actions without coercing or experimenting with
-- | `Exist` here.
-- |
-- | * Probably we could split query and keep `FullQuery`.
-- | * We can add `∀ i o. Query i o` to `PgQueryF`.
data SeldaF a
  = SeldaF (SeldaPG a)
  | PgExecuteF String a
  | PgQueryF (PG.Connection → Aff (Either PGError a))
  | PgOpenTransactionF a
  | PgCloseTransactionF a

derive instance functorSeldaF ∷ Functor SeldaF

type SELDA
  = FProxy SeldaF

type Selda eff
  = ( selda ∷ SELDA | eff )

_selda = SProxy ∷ SProxy "selda"

type PgError r
  = ( pgError ∷ EXCEPT PGError | r )

type PgConnection r
  = ( pg ∷ STATE { conn ∷ Maybe PG.ConnectResult, inTransaction ∷ Boolean, pool ∷ PG.Pool } | r )

_pg = SProxy ∷ SProxy "pg"

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

openTransaction ∷ ∀ eff. Run (Selda + eff) Unit
openTransaction = Run.lift _selda (PgOpenTransactionF unit)

closeTransaction ∷ ∀ eff. Run (Selda + eff) Unit
closeTransaction = Run.lift _selda (PgCloseTransactionF unit)

withTransaction ∷ ∀ a eff. Run (Selda + eff) a → Run (Selda + eff) a
withTransaction block = do
  openTransaction
  r ← block
  closeTransaction
  pure r

pgExecute ∷ ∀ eff. String → Run (Selda + eff) Unit
pgExecute q = Run.lift _selda (PgExecuteF q unit)

pgQuery ∷
  ∀ eff i o.
  PG.ToSQLRow i ⇒
  PG.FromSQLRow o ⇒
  PG.Query i o →
  i →
  Run (Selda + eff) (Array o)
pgQuery q i = Run.lift _selda (PgQueryF (\conn → PG.query conn q i))

run ∷
  ∀ eff.
  Run (AffRow + EffRow + PgConnection + PgError + Selda + eff)
    ~> Run (AffRow + EffRow + PgConnection + PgError + eff)
run action = do
  a ← Run.run (Run.on _selda handleSelda Run.send) action
  getAt _pg >>= _.conn
    >>> case _ of
        Just { connection, done } → do
          inTransaction
            >>= flip when do
                execute "ROLLBACK TRANSACTION"
                modifyAt _pg _ { inTransaction = false }
          Run.liftEffect $ done
          modifyAt _pg _ { conn = Nothing }
        Nothing → pure unit
  pure a
  where
  inTransaction = getAt _pg <#> _.inTransaction

  conn = do
    pg ← getAt _pg
    case pg.conn of
      Nothing →
        (Run.liftAff (PG.connect pg.pool))
          >>= case _ of
              Right result → do
                putAt _pg (pg { conn = Just result })
                pure result.connection
              Left err → throwAt _pgError err
      Just { connection } → pure connection

  execute q = do
    c ← conn
    (Run.liftAff $ PG.execute c (PG.Query q) Row0)
      >>= case _ of
          Just err → throwAt _pgError err
          Nothing → pure unit

  handleSelda ∷ ∀ a. SeldaF a → Run (AffRow + EffRow + PgConnection + PgError + eff) a
  handleSelda = case _ of
    SeldaF q → do
      c ← conn
      Run.liftAff (runReaderT (runExceptT q) c)
        >>= case _ of
            Right next → pure next
            Left err → throwAt _pgError err
    PgExecuteF q next → do
      execute q
      pure next
    PgQueryF q → do
      c ← conn
      (Run.liftAff (q c))
        >>= case _ of
            Left err → throwAt _pgError err
            Right next → pure next
    PgOpenTransactionF next → do
      inTransaction >>= not
        >>> flip when do
            execute "BEGIN TRANSACTION"
            modifyAt _pg _ { inTransaction = true }
      pure next
    PgCloseTransactionF next → do
      inTransaction
        >>= flip when do
            execute "END TRANSACTION"
            modifyAt _pg _ { inTransaction = false }
      pure next

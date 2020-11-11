-- | This module:
-- | * Exposes low level PostgreSql API (internally it is Reader + wrapping around Resource / Aff).
-- | * Uses JS `Pool.query` as a default mode for quering DB.
-- | * Provides a way for managing transaction in a safe manner.
module WebRow.PostgreSQL.PG where

import Prelude
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Resource (Resource, acquire) as Resource
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant.Internal (FProxy)
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, ConnectResult, Connection, PGError(..), Pool, Query(..), Row0(..), Row1, fromClient, fromPool) as PG
import Database.PostgreSQL.Aff (command, connect, execute, query, scalar) as PG
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff) as Aff.Class
import Effect.Class (liftEffect) as Effect.Class
import Effect.Exception (error) as Effect.Exception
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except (throwAt) as Run.Except
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Resource (Resource, liftResource)

foreign import kind TransactionMode

foreign import data Inside ∷ TransactionMode

foreign import data Outside ∷ TransactionMode

-- | TODO:
-- | This is somewhat unsafe meaning we can have
-- | (mode ∷ Inside) but lack the connection value...
-- | Should we really care about this incosistency?
newtype Conn (mode ∷ TransactionMode)
  = Conn (PG.Pool /\ (Maybe PG.Connection))

-- | I'm not sure if it is possible to abstract away
-- | whole pg interaction if I want to preserve
-- | `withTransaction` (it seems that it should contain
-- | `Run` value inside itself - is it possible?).
-- | Because of this problem we provide only tiny a layer
-- | above reader / Aff effect (to limit the damage).
newtype PGF mode a
  = PGF (Conn mode → Resource.Resource (Either PG.PGError a))

derive instance pgFunctor ∷ Functor (PGF mode)

type PG mode
  = FProxy (PGF mode)

type Pg mode r
  = ( pg ∷ PG mode | r )

_pg = SProxy ∷ SProxy "pg"

connection ∷ ∀ mode r. Run (Pg mode + r) PG.Connection
connection = Run.lift _pg (PGF (pure <<< wrap))
  where
  wrap (Conn (p /\ Nothing)) = Right (PG.fromPool p)

  wrap (Conn (_ /\ (Just conn))) = Right conn

pool ∷ ∀ mode r. Run (Pg mode + r) PG.Pool
pool = Run.lift _pg (PGF (pure <<< wrap))
  where
  wrap (Conn (p /\ _)) = Right p

liftPGAff ∷ ∀ a mode r. Aff (Either PG.PGError a) → Run (Pg mode + r) a
liftPGAff action = Run.lift _pg (PGF $ const $ Aff.Class.liftAff action)

liftAff ∷ ∀ a mode r. Aff a → Run (Pg mode + r) a
liftAff action = liftPGAff (map Right action)

liftEffect ∷ ∀ a mode r. Effect a → Run (Pg mode + r) a
liftEffect action = liftAff $ Effect.Class.liftEffect action

execute ::
  ∀ i mode o r.
  PG.ToSQLRow i ⇒
  PG.Query i o →
  i →
  Run (Pg mode + r) Unit
execute q i = do
  conn <- connection
  liftPGAff (map liftErr $ PG.execute conn q i)
  where
  liftErr Nothing = Right unit

  liftErr (Just err) = Left err

query ::
  ∀ i mode o r.
  PG.ToSQLRow i =>
  PG.FromSQLRow o =>
  PG.Query i o ->
  i ->
  Run (Pg mode + r) (Array o)
query q i = do
  conn <- connection
  liftPGAff (PG.query conn q i)

scalar ::
  forall i mode o r.
  PG.ToSQLRow i =>
  PG.FromSQLValue o =>
  PG.Query i (PG.Row1 o) ->
  i ->
  Run (Pg mode + r) (Maybe o)
scalar q i = do
  conn <- connection
  liftPGAff (PG.scalar conn q i)

command ::
  forall i mode r.
  PG.ToSQLRow i =>
  PG.Query i Int ->
  i ->
  Run (Pg mode + r) Int
command q i = do
  conn <- connection
  liftPGAff (PG.command conn q i)

type Commited
  = Boolean

rollback ∷ Ref Commited → Either PG.PGError PG.ConnectResult → Aff Unit
rollback _ (Left err) = pure unit

rollback ref (Right { client, done }) = do
  commited ← Effect.Class.liftEffect $ Ref.read ref
  when (not commited)
    $ do
        void
          $ (PG.execute (PG.fromClient client) (PG.Query "ROLLBACK TRANSACTION") PG.Row0)
              `catchError`
                (const $ pure Nothing)
        -- | I'm swallowing rollback exceptions
        -- | at the moment... Should I rethrow them?
        -- case err of
        --   Just e → rethrow e
        --   Nothing → pure unit
        pure unit
  Effect.Class.liftEffect done
  where
  rethrow ∷ PG.PGError → Aff Unit
  rethrow e =
    throwError
      $ case e of
          PG.ClientError err _ → err
          PG.ConversionError s → Effect.Exception.error s
          PG.InternalError err -> err.error
          PG.OperationalError err -> err.error
          PG.ProgrammingError err -> err.error
          PG.IntegrityError err -> err.error
          PG.DataError err -> err.error
          PG.NotSupportedError err -> err.error
          PG.QueryCanceledError err -> err.error
          PG.TransactionRollbackError err -> err.error

withTransaction ∷ ∀ a r. Run (Pg Inside + r) a → Run (Pg Outside + r) a
withTransaction action = do
  p ← pool
  ref ← liftEffect $ Ref.new false
  { client } ← Run.lift _pg (PGF $ const $ snd <$> (Resource.acquire (connect p) (rollback ref)))
  let
    conn = Conn (p /\ (Just $ PG.fromClient client))

    -- | Run.expand definition is based on `Union` constraint
    -- | We want to use Row.Cons here instead
    expand' ∷ ∀ l b t t_. Row.Cons l b t_ t ⇒ SProxy l → Run t_ ~> Run t
    expand' _ = unsafeCoerce

    handle (PGF k) = Run.send $ Run.inj _pg $ PGF \_ → k conn
  a ← Run.run (Run.on _pg handle (Run.send >>> expand' _pg)) action
  liftPGAff $ map liftErr $ PG.execute (PG.fromClient client) (PG.Query "COMMIT TRANSACTION") PG.Row0
  void $ liftEffect $ Ref.write true ref
  pure a
  where
  liftErr Nothing = Right unit

  liftErr (Just err) = Left err

  connect p = do
    PG.connect p
      >>= case _ of
          err@(Left _) → pure err
          res@(Right { client }) → do
            PG.execute (PG.fromClient client) (PG.Query "BEGIN TRANSACTION") PG.Row0
              >>= case _ of
                  Just err → pure (Left err)
                  Nothing → pure res

type PgExcept r
  = ( pgExcept ∷ EXCEPT PG.PGError | r )

_pgExcept = SProxy ∷ SProxy "pgExcept"

run ∷ ∀ a r. PG.Pool → Run (Pg Outside + PgExcept + Resource + r) a → Run (PgExcept + Resource + r) a
run p action = Run.run (Run.on _pg handle Run.send) action
  where
  handle (PGF k) = do
    let
      conn = Conn (p /\ Nothing)
    liftResource (k conn)
      >>= case _ of
          Right next → pure next
          Left err → Run.Except.throwAt _pgExcept err

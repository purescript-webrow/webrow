-- | This module:
-- | * Exposes low level PostgreSql API (internally it is Reader + wrapping around Resource / Aff).
-- | * Uses JS `Pool.query` as a default mode for quering DB.
-- | * Provides a way for managing transaction in a safe manner.
module WebRow.PostgreSQL.PG
  ( module Exports
  , command
  , execute
  , scalar
  , _pgExcept
  , PGEXCEPT
  , query
  , run
  , withTransaction
  )
  where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Resource (acquire) as Resource
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, ConnectResult, PGError(..), Pool, Query(..), Row0(..), Row1, fromClient) as PG
import Database.PostgreSQL.Aff (command, connect, execute, query, scalar) as PG
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect.Class
import Effect.Exception (error) as Effect.Exception
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Prim.Row (class Cons) as Row
import Run (Run)
import Run as Run
import Run.Except (Except)
import Run.Except (throwAt) as Run.Except
import Type.Row (type (+))
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.PostgreSQL.Internal (Conn(..), Inside, Outside, PG, Pg(..), _pg, connection, liftEffect, liftPgAff, pool)
import WebRow.PostgreSQL.Internal (Pg, PG, _pg, Inside, Outside, TransactionMode) as Exports
import WebRow.Resource (RESOURCE, liftResource)

execute ::
  ∀ i mode o r.
  PG.ToSQLRow i ⇒
  PG.Query i o →
  i →
  Run (PG mode + r) Unit
execute q i = do
  conn <- connection
  liftPgAff (map liftErr $ PG.execute conn q i)
  where
  liftErr Nothing = Right unit

  liftErr (Just err) = Left err

query ::
  ∀ i mode o r.
  PG.ToSQLRow i =>
  PG.FromSQLRow o =>
  PG.Query i o ->
  i ->
  Run (PG mode + r) (Array o)
query q i = do
  conn <- connection
  liftPgAff (PG.query conn q i)

scalar ::
  forall i mode o r.
  PG.ToSQLRow i =>
  PG.FromSQLValue o =>
  PG.Query i (PG.Row1 o) ->
  i ->
  Run (PG mode + r) (Maybe o)
scalar q i = do
  conn <- connection
  liftPgAff (PG.scalar conn q i)

command ::
  forall i mode r.
  PG.ToSQLRow i =>
  PG.Query i Int ->
  i ->
  Run (PG mode + r) Int
command q i = do
  conn <- connection
  liftPgAff (PG.command conn q i)

type Commited
  = Boolean

rollback ∷ Ref Commited → Either PG.PGError PG.ConnectResult → Aff Unit
rollback _ (Left _) = pure unit

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

withTransaction ∷ ∀ a r. Run (PG Inside + r) a → Run (PG Outside + r) a
withTransaction action = do
  p ← pool
  ref ← liftEffect $ Ref.new false
  { client } ← Run.lift _pg (Pg $ const $ snd <$> (Resource.acquire (connect p) (rollback ref)))
  let
    conn = Conn (p /\ (Just $ PG.fromClient client))

    -- | Run.expand definition is based on `Union` constraint
    -- | We want to use `Row.Cons` constraint here instead.
    expand' ∷ ∀ l b t t_. Row.Cons l b t_ t ⇒ Proxy l → Run t_ ~> Run t
    expand' _ = unsafeCoerce

    handle (Pg k) = Run.send $ Run.inj _pg $ Pg \_ → k conn
  a ← Run.run (Run.on _pg handle (Run.send >>> expand' _pg)) action
  liftPgAff $ map liftErr $ PG.execute (PG.fromClient client) (PG.Query "COMMIT TRANSACTION") PG.Row0
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

type PGEXCEPT r = ( pgExcept ∷ Except PG.PGError | r )

_pgExcept = Proxy ∷ Proxy "pgExcept"

run ∷ ∀ a r. PG.Pool → Run (PG Outside + PGEXCEPT + RESOURCE + r) a → Run (PGEXCEPT + RESOURCE + r) a
run p action = Run.run (Run.on _pg handle Run.send) action
  where
  handle (Pg k) = do
    let
      conn = Conn (p /\ Nothing)
    liftResource (k conn)
      >>= case _ of
          Right next → pure next
          Left err → Run.Except.throwAt _pgExcept err

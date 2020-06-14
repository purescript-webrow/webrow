module WebRow.Selda.App where

import Prelude

import Data.Either (Either(..))
import Database.PostgreSQL as PG
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import WebRow.Selda.CLI (getOptions)

setupDB ∷ ∀ a. (PG.Connection → Aff a) → Aff a
setupDB k = (\k' → k' k) =<< liftEffect do
  opts ← getOptions
  pool ← PG.newPool opts.database
  pure $ withConn pool

withConn ∷ ∀ a. PG.Pool → (PG.Connection → Aff a) → Aff a
withConn pool k = PG.withConnection pool case _ of
  Left pgError → onErr "PostgreSQL connection error: " pgError
  Right conn → do
    -- | setup the connection, set schema, timezone, etc.
    -- void $ PG.execute conn (PG.Query $ "set schema '" <> opts.schema <> "';") PG.Row0
    -- XXX: important - setting the timezone influences the results of extract function
    void $ PG.execute conn (PG.Query $ "set timezone TO 'GMT';") PG.Row0
    k conn

onErr ∷ ∀ a b. Show a ⇒ String → a → Aff b
onErr msg e = throwError $ Aff.error $ msg <> show e


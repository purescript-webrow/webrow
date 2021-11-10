module Test.WebRow.PostgreSQL.PG where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (PGError, Pool, fromPool)
import Database.PostgreSQL.Aff (query) as PostgreSQL.Aff
import Database.PostgreSQL.Pool (idleCount, totalCount) as Pool
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error) as Effect.Exception
import JS.Unsafe.Stringify (unsafeStringify)
import Run (AFF, Run, EFFECT)
import Run (liftAff, liftEffect) as Run
import Run.Except (catchAt, throwAt)
import Selda (Table(..))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual) as Assertions
import Test.Spec.Assertions (shouldEqual) as Spec
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.PostgreSQL (Outside, PGEXCEPT, Pg, Query(..), Row0(..), Row1, Row3(..), PG, _pgExcept)
import WebRow.PostgreSQL (run) as PG
import WebRow.PostgreSQL.PG (execute, query) as PG
import WebRow.PostgreSQL.PG (withTransaction)
import WebRow.Resource (RESOURCE, runBaseResource')
import WebRow.Testing.Assertions (shouldEqual)

type PeopleRow = ( age ∷ Maybe Int, id ∷ Int, name ∷ String )
type Person = { | PeopleRow }

people ∷ Table PeopleRow
people = Table { name: "people" }

type PersonPGRow = Row3 Int String (Maybe Int)

_testErr = SProxy ∷ SProxy "testErr"

initDb ∷ ∀ eff mode. Run (PG mode + eff) Unit
initDb =
  do
    let
      sql ∷ Query Row0 Row0
      sql = Query """
        DROP TABLE IF EXISTS people;
        CREATE TABLE people (
          id INTEGER PRIMARY KEY,
          name TEXT NOT NULL,
          age INTEGER
        );

        DO $$
        BEGIN
          IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'account_type') THEN
            CREATE TYPE ACCOUNT_TYPE as ENUM (
              'business',
              'personal'
            );
          END IF;
        END$$;

        DROP TABLE IF EXISTS bank_accounts;
        CREATE TABLE bank_accounts (
          id INTEGER PRIMARY KEY,
          "personId" INTEGER NOT NULL,
          balance INTEGER NOT NULL,
          "accountType" ACCOUNT_TYPE NOT NULL
        );
      """
    PG.execute sql Row0


runTest ∷ ∀ a. Pool → Run (AFF + EFFECT + PG Outside + PGEXCEPT + RESOURCE + ()) a → Aff a
runTest pool action =
  runBaseResource'
  <<< catchAt _pgExcept (\e → Run.liftEffect $ throwError $ Effect.Exception.error (unsafeStringify e))
  <<< PG.run pool
  $ do
    initDb
    action

spec ∷ ∀ m. Monad m ⇒ Pool → SpecT Aff Unit m Unit
spec pool = do
  describe "WebRow.PostgreSQL.PG" do
    it "executes statements correctly" do
      runTest pool do
        p ← PG.query (Query "SELECT id from people") Row0
        p `shouldEqual` ([] ∷ Array (Row1 Int))

    it "commits after transaction" do
      runTest pool do
        withTransaction $ do
          PG.execute (Query "INSERT into people (id, name, age) VALUES (1, 'foo', NULL)") Row0

        p ← PG.query (Query "SELECT id, name, age from people") Row0
        p `shouldEqual` ([ Row3 1 "foo" Nothing ] ∷ Array (Row3 Int String (Maybe Int)))

      totalCount ← liftEffect $ Pool.totalCount pool
      idleCount ← liftEffect $ Pool.idleCount pool
      idleCount `Spec.shouldEqual` totalCount

    it "rollbacks on exception" do
      let
        run' action
          = runBaseResource'
          <<< catchAt _pgExcept (\e → Run.liftEffect $ throwError $ Effect.Exception.error (unsafeStringify e))
          -- <<< catchAt _testErr (\e → Run.liftEffect $ throwError $ Effect.Exception.error (unsafeStringify e))
          <<< catchAt _testErr (const $ pure unit)
          <<< PG.run pool
          $ do
            initDb
            action

      run' do
        void $ withTransaction $ do
          PG.execute (Query "INSERT into people (id, name, age) VALUES (1, 'foo', NULL)") Row0
          throwAt _testErr "throw before COMMIT"

      (p ∷ Either _ (Array PersonPGRow)) ← PostgreSQL.Aff.query (fromPool pool) (Query "SELECT id, name, age from people") Row0
      p `Assertions.shouldEqual` (Right [])

      totalCount ← liftEffect $ Pool.totalCount pool
      idleCount ← liftEffect $ Pool.idleCount pool
      idleCount `Spec.shouldEqual` totalCount

    it "rollbacks on aff exception" do
      let
        run' action
          = action' `catchError` \e → pure unit
          where
            action' = runBaseResource'
              <<< catchAt _pgExcept (\e → Run.liftEffect $ throwError $ Effect.Exception.error (unsafeStringify e))
              <<< catchAt _testErr (const $ pure unit)
              <<< PG.run pool
              $ do
                initDb
                action

      run' do
        void $ withTransaction $ do
          PG.execute (Query "INSERT into people (id, name, age) VALUES (1, 'foo', NULL)") Row0
          void $ Run.liftAff $ liftEffect $ throwError $ Effect.Exception.error "Aff throw before commit"

      (p ∷ Either _ (Array PersonPGRow)) ← PostgreSQL.Aff.query (fromPool pool) (Query "SELECT id, name, age from people") Row0
      p `Assertions.shouldEqual` (Right [])

      totalCount ← liftEffect $ Pool.totalCount pool
      idleCount ← liftEffect $ Pool.idleCount pool
      idleCount `Spec.shouldEqual` totalCount


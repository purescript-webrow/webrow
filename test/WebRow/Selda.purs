module Test.WebRow.Selda where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool) as PG
import Debug.Trace (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error) as Effect.Exception
import Effect.Exception (throw)
import Global.Unsafe (unsafeStringify)
import Run (liftAff, liftEffect) as Run
import Run (runBaseAff')
import Run.Except (catchAt, throwAt)
import Selda (Table(..), aggregate, count, lit, restrict, selectFrom, (.==))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail)
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Selda (_pgError, insert1_, pgExecute, query, query1, withTransaction)
import WebRow.Selda (run) as Selda
import WebRow.Testing.Assertions (shouldEqual)

type PeopleRow = ( age ∷ Maybe Int, id ∷ Int, name ∷ String )
type Person = { | PeopleRow }

people ∷ Table PeopleRow
people = Table { name: "people" }

_testErr = SProxy ∷ SProxy "testErr"

spec :: forall m. Monad m => PG.Pool -> SpecT Aff Unit m Unit
spec pool = do
  let
    initDb = """
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


  describe "WebRow.Selda" do
    let
      run action =
        runBaseAff'
        <<< catchAt _pgError (\e → Run.liftEffect $ throwError $ Effect.Exception.error (unsafeStringify e))
        <<< Selda.run pool
        $ action

    it "executes statements correctly" do
      run do
        pgExecute initDb
        p ← query $ selectFrom people pure
        p `shouldEqual` []

    it "commits after transaction" do
      run do
        pgExecute initDb
        traceM "AFTER INIT"
        withTransaction $ do
          insert1_ people { id: 1, name: "paluh", age: Nothing }
        pure unit

      run do
        n <- query1 $ aggregate $ selectFrom people \r → pure { n: count r.id }
        n `shouldEqual` (Just { n: 1 })

    it "rollbacks on aff exception" do
      let
        run' action =
          let
            action' = run action
          in
            action' `catchError` \e → do
              traceM e
              pure unit
      run' do
        pgExecute initDb
        withTransaction $ do
          void $ insert1_ people { id: 1, name: "foo", age: Nothing }

        withTransaction $ do
          insert1_ people { id: 2, name: "bar", age: Nothing }
          Run.liftAff $ liftEffect $ throwError $ Effect.Exception.error "Aff throw before commit"

      run' do
        n <- query1 $ aggregate $ selectFrom people \r → pure { n: count r.id }
        n `shouldEqual` (Just { n: 1 })

    it "rollbacks on error" do
      let
        run' action = run $ catchAt _testErr (const $ pure unit) $ action

      run' do
        traceM "BEFORE INIT"
        pgExecute initDb
        withTransaction $ do
          insert1_ people { id: 1, name: "foo", age: Nothing }

        withTransaction $ do
          void $ insert1_ people { id: 2, name: "bar", age: Nothing }
          throwAt _testErr "throw before COMMIT"

      run' do
        n <- query1 $ aggregate $ selectFrom people \r → pure { n: count r.id }
        n `shouldEqual` (Just { n: 1 })



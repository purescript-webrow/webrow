module Test.WebRow.Selda where

import Prelude

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool) as PG
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Run (runBaseAff')
import Run (liftEffect) as Run
import Run.Except (catchAt, throwAt)
import Selda (Table(..), aggregate, count, selectFrom)
import Test.Spec (SpecT, describe, it)
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
    it "executes statements correctly" do
      runBaseAff'
        <<< catchAt _pgError (const $ pure unit)
        <<< Selda.run pool $ do
          pgExecute initDb
          p ← query $ selectFrom people pure
          p `shouldEqual` []

          -- liftEffect $ log $ "NUMBER: " <> show number
      pure unit

    it "commits after transaction" do
      runBaseAff'
        <<< catchAt _pgError (unsafeCoerce >>> throw >>> Run.liftEffect)
        <<< catchAt _testErr (const $ pure unit)
        <<< Selda.run pool $ do
          pgExecute initDb
          void $ withTransaction $ do
            insert1_ people { id: 1, name: "paluh", age: Nothing }
          pure unit

      runBaseAff'
        <<< catchAt _pgError (unsafeCoerce >>> throw >>> Run.liftEffect)
        <<< catchAt _testErr (const $ pure unit)
        <<< Selda.run pool $ do
          { n } <- query1 $ aggregate $ selectFrom people \r → pure { n: count r.id }
          n `shouldEqual` 1

    it "rollbacks on error" do
      runBaseAff'
        <<< catchAt _pgError (\err → pure unit)
        -- <<< catchAt _pgError (\err → Run.liftEffect do
        --   log "WTF"
        --   traceM err
        --   throw $ unsafeCoerce err)
        <<< catchAt _testErr (const $ pure unit)
        <<< Selda.run pool $ do
          pgExecute initDb
          void $ withTransaction $ do
            void $ insert1_ people { id: 1, name: "foo", age: Nothing }

          void $ withTransaction $ do
            void $ insert1_ people { id: 2, name: "bar", age: Nothing }
            throwAt _testErr "throw before COMMIT"
          pure unit

      runBaseAff'
        <<< catchAt _pgError (unsafeCoerce >>> throw >>> Run.liftEffect)
        <<< catchAt _testErr (const $ pure unit)
        <<< Selda.run pool $ do
          { n } <- query1 $ aggregate $ selectFrom people \r → pure { n: count r.id }
          n `shouldEqual` 1


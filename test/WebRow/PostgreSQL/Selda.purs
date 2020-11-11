module Test.WebRow.Selda where

import Prelude
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool) as PG
import Effect.Aff (Aff)
import Selda (selectFrom)
import Test.Spec (SpecT, describe, it)
import Test.WebRow.PostgreSQL.PG (people, runTest)
import WebRow.PostgreSQL (withTransaction)
import WebRow.PostgreSQL.Selda (insert1_, query, query1)
import WebRow.Testing.Assertions (shouldEqual)

spec :: forall m. Monad m => PG.Pool -> SpecT Aff Unit m Unit
spec pool = do
  describe "WebRow.Selda" do
    it "performs trivial select" do
      runTest pool do
        p ← query $ selectFrom people pure
        p `shouldEqual` []
    it "performs insert outside of transaction block" do
      let
        row = { id: 1, name: "foo", age: Nothing }
      runTest pool do
        insert1_ people row
        p ← query1 $ selectFrom people pure
        p `shouldEqual` (Just row)
    it "commits insert within transaction" do
      let
        row = { id: 1, name: "foo", age: Nothing }
      runTest pool do
        withTransaction
          $ insert1_ people row
        p ← query1 $ selectFrom people pure
        p `shouldEqual` (Just row)

module WebRow.Contrib.Run.Reader where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (class VariantFMatchCases)
import Prim.Row (class Union)
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList (Cons, Nil) as RL
import Prim.Row (class Cons, class Lacks) as Row
import Run (Run)
import Run.Reader (Reader(..))
import Run as Run
import Record (insert, get) as Record
import Type.Prelude (class IsSymbol, SProxy(..), RLProxy(..))


-- -- | Example usage
-- 
-- -- | This is for sure ugly but could be probably
-- -- | constructed from plain value record `{ x: 8, y: "test" }`
-- context = { x: 8, y: "test" }
-- 
-- usingReaders :: forall eff. Run (x :: READER Int, y :: READER String | eff) String
-- usingReaders = do
--   i ← askAt (SProxy ∷ SProxy "x")
--   s ← askAt (SProxy ∷ SProxy "y")
--   pure $ s <> show i
-- 
-- interpreted :: forall eff. Run eff String
-- interpreted = runReaders context usingReaders



-- | Turn record of values into a record of
-- | `Reader k` applications so we can use it
-- | when dispaching readers over the context.
-- |
-- | { x: 8 }
-- |
-- | into
-- |
-- | { x: \(Reader k) → Left (k 8) }
-- |
class RunReaders (il ∷ RowList) (i ∷ # Type) (o ∷ # Type) | il → o where
  runReadersImpl ∷ RLProxy il → { | i } → { | o }

instance runReadersNil ∷ RunReaders RL.Nil i () where
  runReadersImpl _ _ = {}

instance runReadersCons ∷
  ( IsSymbol s
  , Row.Lacks s o_
  , RunReaders t i o_
  , Row.Cons s e i_ i
  , Row.Cons s (Reader e a → Either a b) o_ o
  , RunReaders t i o_
  ) ⇒ RunReaders (RL.Cons s e t) i o where
  runReadersImpl _ i =
    let
      l = SProxy ∷ SProxy s
      o_ = runReadersImpl (RLProxy ∷ RLProxy t) i
    in
      Record.insert l (\(Reader k) → Left (k (Record.get l i))) o_

runReaders ∷
  ∀ r r' rl rl' r1 r2 r3 a
  . RowToList r rl
  ⇒ RunReaders rl r r'
  ⇒ RowToList r' rl'
  ⇒ VariantFMatchCases rl' r1 (Run r3 a) (Either (Run r3 a) (Run r2 (Run r3 a)))
  ⇒ Union r1 r2 r3
  ⇒ Record r
  → Run r3 a
  → Run r2 a
runReaders e = loop
  where
  e' = runReadersImpl (RLProxy ∷ RLProxy rl) e
  handle = Run.onMatch e' (Run.send >>> Right)
  loop r = case Run.peel r of
    Left a → case handle a of
      Left k → loop k
      Right a' → a' >>= loop
    Right a → pure a


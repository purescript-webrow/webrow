module WebRow.Contrib.Run.Reader where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (inj, class VariantFMatchCases)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Union)
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Record (insert, get) as Record
import Run (Run)
import Run (on, onMatch, peel, run, send) as Run
import Run.Reader (READER, Reader(..))
import Type.Prelude (class IsSymbol, Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

withReaderAt ∷
  ∀ e1 e2 r_ r1 r2 s
  . IsSymbol s
  ⇒ Row.Cons s (Reader e1) r_ r1
  ⇒ Row.Cons s (Reader e2) r_ r2
  ⇒ Proxy s
  → (e2 → e1)
  → Run r1
  ~> Run r2
withReaderAt sym f r = Run.run (Run.on sym handle (Run.send >>> expand' sym)) r
  where
    expand' ∷ ∀ l b t t_. Row.Cons l b t_ t ⇒ Proxy l → Run t_ ~> Run t
    expand' _ = unsafeCoerce

    handle (Reader k) = Run.send (inj sym (Reader (k <<< f)))



-- -- | Example usage
--
-- -- | This is for sure ugly but could be probably
-- -- | constructed from plain value record `{ x: 8, y: "test" }`
-- context = { x: 8, y: "test" }
-- 
-- usingReaders :: forall eff. Run (x :: READER Int, y :: READER String | eff) String
-- usingReaders = do
--   i ← askAt (Proxy ∷ Proxy "x")
--   s ← askAt (Proxy ∷ Proxy "y")
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
class RunReaders (il ∷ RowList Type) (i ∷ Row Type) (o ∷ Row Type) | il → o where
  runReadersImpl ∷ Proxy il → { | i } → { | o }

instance runReadersNil ∷ RunReaders RL.Nil i () where
  runReadersImpl _ _ = {}

instance runReadersCons ∷
  ( IsSymbol s
  , Row.Lacks s o_
  , RunReaders t i o_
  , Row.Cons s e i_ i
  , Row.Cons s (Reader e a → Either a b) o_ o
  , RunReaders t i o_
  ) ⇒
  RunReaders (RL.Cons s e t) i o where
  runReadersImpl _ i =
    let
      l = Proxy ∷ Proxy s

      o_ = runReadersImpl (Proxy ∷ Proxy t) i
    in
      Record.insert l (\(Reader k) → Left (k (Record.get l i))) o_

runReaders ∷
  ∀ r r' rl rl' r1 r2 r3 a.
  RowToList r rl ⇒
  RunReaders rl r r' ⇒
  RowToList r' rl' ⇒
  VariantFMatchCases rl' r1 (Run r3 a) (Either (Run r3 a) (Run r2 (Run r3 a))) ⇒
  Union r1 r2 r3 ⇒
  Record r →
  Run r3 a →
  Run r2 a
runReaders e = loop
  where
  e' = runReadersImpl (Proxy ∷ Proxy rl) e

  handle = Run.onMatch e' (Run.send >>> Right)

  loop r = case Run.peel r of
    Left a → case handle a of
      Left k → loop k
      Right a' → a' >>= loop
    Right a → pure a

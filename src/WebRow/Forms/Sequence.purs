module WebRow.Forms.Sequence where

import Prelude

import Control.Monad.State (evalState)
import Control.Monad.State (modify) as State
import Control.Monad.State.Trans (StateT)
import Data.Identity (Identity)

newtype Sequence a = Sequence (StateT Int Identity a)
derive newtype instance functorSequence ∷ Functor Sequence
derive newtype instance applySequence ∷ Apply Sequence
derive newtype instance applicativeSequence ∷ Applicative Sequence
derive newtype instance bindSequence ∷ Bind Sequence
derive newtype instance monadSequence ∷ Monad Sequence

next ∷ Sequence Int
next = Sequence $ State.modify (add 1)

eval ∷ ∀ a. Sequence a → a
eval (Sequence a) = evalState a 0

id ∷ Sequence String
id = append "id:" <<< show <$> next


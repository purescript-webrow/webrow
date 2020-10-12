module WebRow.Forms.BuilderM where

import Prelude
import Control.Monad.State (evalState)
import Control.Monad.State (modify) as State
import Control.Monad.State.Trans (StateT)
import Data.Identity (Identity)

-- | TODO: Add Reader to the stack with "form id" prefix.
-- | It is needed when multiple forms are built separately and
-- | displayed on the same site.
newtype BuilderM a
  = BuilderM (StateT Int Identity a)

derive newtype instance functorBuilderM ∷ Functor BuilderM

derive newtype instance applyBuilderM ∷ Apply BuilderM

derive newtype instance applicativeBuilderM ∷ Applicative BuilderM

derive newtype instance bindBuilderM ∷ Bind BuilderM

derive newtype instance monadBuilderM ∷ Monad BuilderM

next ∷ BuilderM Int
next = BuilderM $ State.modify (add 1)

eval ∷ ∀ a. BuilderM a → a
eval (BuilderM a) = evalState a 0

id ∷ BuilderM String
id = append "id:" <<< show <$> next

module WebRow.Forms.Uni.Builder where

import Prelude

import Data.Newtype (class Newtype)
import Polyform (Reporter)
import WebRow.Forms.BuilderM (BuilderM)

-- | Just a `Reporter` but with predefined
-- | default layout.
newtype Builder m layout i o = Builder
  ( BuilderM
      { default ∷ m layout
      , reporter ∷ Reporter m layout i o
      }
  )
derive instance newtypeBuilder ∷ Newtype (Builder m l i o) _
derive instance functorBuilder ∷ (Applicative m) ⇒ Functor (Builder m layout i)

instance applyBuilder ∷ (Applicative m, Semigroup layout) ⇒ Apply (Builder m layout i) where
  apply (Builder sw1) (Builder sw2) = Builder $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { default: append <$> w1.default <*> w2.default
      , reporter: apply w1.reporter w2.reporter
      }
instance applicativeBuilder ∷ (Applicative m, Monoid layout) ⇒ Applicative (Builder m layout i) where
  pure a = Builder $ do
    pure
      { default: pure mempty
      , reporter: pure a
      }

instance semigroupoidBuilder ∷ (Monad m, Semigroup layout) ⇒ Semigroupoid (Builder m layout) where
  compose (Builder sw1) (Builder sw2) = Builder $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { default: append <$> w1.default <*> w2.default
      , reporter: compose w1.reporter w2.reporter
      }

instance categoryBuilder ∷ (Monad m, Monoid layout) ⇒ Category (Builder m layout) where
  identity = Builder $ pure
    { default: pure mempty
    , reporter: identity
    }


module WebRow.Forms.Uni.Builder where

import Prelude
import Data.Newtype (class Newtype)
import Polyform (Reporter)
import WebRow.Forms.BuilderM (BuilderM)

-- | We should differenciate monad here
-- | for default and reporter so we can
-- | build default layout without
-- | using heavy context.
newtype Builder m layout i o
  = Builder
  ( BuilderM
      { default ∷ layout
      , reporter ∷ Reporter m layout i o
      }
  )

derive instance newtypeBuilder ∷ Newtype (Builder m l i o) _

derive instance functorBuilder ∷ (Applicative m) ⇒ Functor (Builder m layout i)

instance applyBuilder ∷ (Monad m, Monoid layout) ⇒ Apply (Builder m layout i) where
  apply (Builder sw1) (Builder sw2) =
    Builder
      $ do
          w1 ← sw1
          w2 ← sw2
          pure
            { default: w1.default <> w2.default
            , reporter: apply w1.reporter w2.reporter
            }

instance applicativeBuilder ∷ (Monad m, Monoid layout) ⇒ Applicative (Builder m layout i) where
  pure a =
    Builder
      $ do
          pure
            { default: mempty
            , reporter: pure a
            }

instance semigroupoidBuilder ∷ (Monad m, Monoid layout) ⇒ Semigroupoid (Builder m layout) where
  compose (Builder sw1) (Builder sw2) =
    Builder
      $ do
          w1 ← sw1
          w2 ← sw2
          pure
            { default: w1.default <> w2.default
            , reporter: compose w1.reporter w2.reporter
            }

instance categoryBuilder ∷ (Monad m, Monoid layout) ⇒ Category (Builder m layout) where
  identity =
    Builder
      $ pure
          { default: mempty
          , reporter: identity
          }

module WebRow.Forms.Bi.Builder where

import Prelude
import Data.Newtype (class Newtype, un)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Polyform (Dual(..))
import Polyform.Batteries.UrlEncoded (Query) as UrlEncoded
import Polyform.Reporter.Dual (DualD, Dual) as Reporter
import WebRow.Forms.BuilderM (BuilderM)

type Default layout
  = { layout ∷ layout
    , payload ∷ UrlEncoded.Query
    }

-- | I'm not sure if we want to carry this `n`
-- | parameter. This disticion is useful
-- | in some scenarios but is it worth such
-- | a type complication?
newtype BuilderD m n layout i o' o
  = BuilderD
  ( BuilderM
      { dualD ∷ Reporter.DualD m layout i o' o
      , default ∷ n (Default layout)
      }
  )

derive instance functorBuilderD ∷ Functor m ⇒ Functor (BuilderD m n layout i o')

instance applyBuilderD ∷ (Monoid layout, Semigroup i, Monad m, Monad n) ⇒ Apply (BuilderD m n layout i o') where
  apply (BuilderD sw1) (BuilderD sw2) =
    BuilderD
      $ do
          w1 ← sw1
          w2 ← sw2
          pure
            { dualD: apply w1.dualD w2.dualD
            , default: append <$> w1.default <*> w2.default
            }

instance applicativeBuilderD ∷
  (Monoid i, Monoid layout, Monad m, Monad n) ⇒
  Applicative (BuilderD m n layout i o') where
  pure a =
    BuilderD
      $ pure
          { dualD: pure a
          , default: pure mempty
          }

instance profunctorBuilderD ∷
  (Functor m) ⇒
  Profunctor (BuilderD m n layout i) where
  dimap l r (BuilderD w) =
    BuilderD do
      { dualD, default: def } ← w
      pure { dualD: dimap l r dualD, default: def }

newtype Builder m n layout i o
  = Builder (BuilderD m n layout i o o)

derive instance newtypeBuilder ∷ Newtype (Builder m n layout i o) _

instance semigroupoidBuilder ∷ (Monoid layout, Monad m, Monad n) ⇒ Semigroupoid (Builder m n layout) where
  compose (Builder (BuilderD sw1)) (Builder (BuilderD sw2)) =
    Builder $ BuilderD
      $ do
          w1 ← sw1
          w2 ← sw2
          pure
            { dualD: un Dual (compose (Dual w1.dualD) (Dual w2.dualD))
            , default: append <$> w1.default <*> w2.default
            }

instance categoryBuilder ∷ (Monoid layout, Monad m, Monad n) ⇒ Category (Builder m n layout) where
  identity = Builder $ BuilderD $ pure { dualD: un Dual identity, default: pure mempty }

infixl 5 diverge as ~

diverge ∷
  ∀ layout i m n o o'.
  Functor m ⇒
  (o' → o) →
  Builder m n layout i o →
  BuilderD m n layout i o' o
diverge f = lcmap f <<< un Builder

fromDual ∷
  ∀ i layout m n o.
  Monoid layout ⇒
  Applicative m ⇒
  Applicative n ⇒
  Reporter.Dual m layout i o →
  Builder m n layout i o
fromDual (Dual d) = Builder $ BuilderD (pure { default: pure mempty, dualD: d })

builder ∷
  ∀ i layout m n o.
  BuilderM
    { default ∷ n (Default layout)
    , dualD ∷ Reporter.DualD m layout i o o
    } →
  Builder m n layout i o
builder = Builder <<< BuilderD

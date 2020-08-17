module WebRow.Forms.Bi.Builder where

import Prelude

import Data.Newtype (class Newtype, un)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Polyform (Dual(..))
import Polyform.Reporter.Dual (DualD) as Reporter
import WebRow.Forms.BuilderM (BuilderM)

newtype BuilderD m layout i o' o = BuilderD
  ( BuilderM
      { dualD ∷ Reporter.DualD m layout i o' o
      , default ∷ m layout
      }
  )

derive instance functorBuilderD ∷ Functor m ⇒ Functor (BuilderD m layout i o')

instance applyBuilderD ∷ (Monoid layout, Semigroup i, Monad m) ⇒ Apply (BuilderD m layout i o') where
  apply (BuilderD sw1) (BuilderD sw2) = BuilderD $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { dualD: apply w1.dualD w2.dualD
      , default: append <$> w1.default <*> w2.default
      }

instance applicativeBuilderD
  ∷ ( Monoid i, Monoid layout, Monad m )
  ⇒ Applicative (BuilderD m layout i o') where
  pure a = BuilderD $ pure
    { dualD: pure a
    , default: pure mempty
    }

instance profunctorBuilderD
  ∷ (Functor m) ⇒ Profunctor (BuilderD m layout i) where
  dimap l r (BuilderD w) = BuilderD do
    { dualD, default: def } ← w
    pure { dualD: dimap l r dualD, default: def }

newtype Builder m layout i o = Builder (BuilderD m layout i o o)
derive instance newtypeBuilder ∷ Newtype (Builder m layout i o) _

instance semigroupoidBuilder ∷ (Monoid layout, Monad m) ⇒ Semigroupoid (Builder m layout) where
  compose (Builder (BuilderD sw1)) (Builder (BuilderD sw2)) = Builder $ BuilderD $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { dualD: un Dual (compose (Dual w1.dualD) (Dual w2.dualD))
      , default: append <$> w1.default <*> w2.default
      }

instance categoryBuilder ∷ (Monoid layout, Monad m) ⇒ Category (Builder m layout) where
  identity = Builder $ BuilderD $ pure { dualD: un Dual identity, default: pure mempty }

infixl 5 diverge as ~

diverge
  ∷ ∀ layout i m o o'
  . Functor m
  ⇒ (o' → o)
  → Builder m layout i o
  → BuilderD m layout i o' o
diverge f = lcmap f <<< un Builder

builder ∷ ∀ i layout m o.
  BuilderM
    { default ∷ m layout
    , dualD ∷ Reporter.DualD m layout i o o
    }
  → Builder m layout i o
builder = Builder <<< BuilderD


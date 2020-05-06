module WebRow.Forms.Plain where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object (lookup) as Object
import Polyform.Reporter (Reporter, report, toEither) as Polyform.Reporter
import Polyform.Reporter (runReporter)
import WebRow.Forms.Payload (Key, Value, UrlDecoded) as Payload
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Validation.Report (Builder) as Validation.Report
import WebRow.Forms.Validation.Report (Key, Result, Report) as Report
import WebRow.Utils.Foreign.Object.Builder (build) as Object.Builder

-- | `step` is a representation of validation step result
newtype Form m layout result i o = Form
  { layout ∷ layout
  , reporter ∷ Polyform.Reporter.Reporter m (Validation.Report.Builder result) i o
  }
derive instance newtypeForm ∷ Newtype (Form m l s i o) _
derive instance functorForm ∷ Applicative m ⇒ Functor (Form m s l i)
instance applicativeForm ∷ (Applicative m, Semigroup layout) ⇒ Apply (Form m layout step i) where
  apply (Form w1) (Form w2) = Form
    { reporter: apply w1.reporter w2.reporter
    , layout: w1.layout <> w2.layout
    }

instance semigroupoidForm ∷ (Monad m, Semigroup layout) ⇒ Semigroupoid (Form m layout step) where
  compose (Form w1) (Form w2) = Form
    { reporter: compose w1.reporter w2.reporter
    , layout: w1.layout <> w2.layout
    }


type ValidatedFormBase layout step a =
  { layout ∷ layout (Maybe Payload.Value) (Maybe (Report.Result step))
  , report ∷ Report.Report step
  | a
  }

type ValidatedForm layout step a =
  Either (ValidatedFormBase layout step ()) (ValidatedFormBase layout step ( result ∷ a ))

run
  ∷ ∀ layout m o step
  . Bifunctor layout
  ⇒ Monad m
  ⇒ Form m (layout Payload.Key Report.Key) step UrlDecoded o
  → UrlDecoded
  → m (ValidatedForm layout step o)
run (Form { layout, reporter }) input = do
  result ← runReporter reporter input
  let
    report = Object.Builder.build (Polyform.Reporter.report result)
  let
    layout' = bimap (flip Map.lookup input) (flip Object.lookup report) layout
  pure $ case Polyform.Reporter.toEither result of
    Right a → Right { layout: layout', report, result: a }
    Left _ → Left { layout: layout', report }

prefill
  ∷ ∀ layout m i o r result step
  . Bifunctor layout
  ⇒ Form m (layout Payload.Key r) step i o
  → Payload.UrlDecoded
  → layout (Maybe Payload.Value) (Maybe result)
prefill (Form { layout }) input = bimap (flip Map.lookup input) (const $ Nothing) layout

-- | Provided to simplify inference
prefill'
  ∷ ∀ layout m i o r result step
  . Bifunctor layout
  ⇒ Applicative m
  ⇒ Form m (layout Payload.Key r) step i o
  → Payload.UrlDecoded
  → m (layout (Maybe Payload.Value) (Maybe result))
prefill' form input = pure (prefill form input)

module WebRow.Forms.Plain where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object (lookup) as Object
import Polyform.Reporter (Reporter, report, toEither) as Polyform.Reporter
import Polyform.Reporter (runReporter)
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

run
  ∷ ∀ i layout m o step
  . Functor layout
  ⇒ Monad m
  ⇒ Form m (layout Report.Key) step i o
  → i
  → m
    { layout ∷ layout (Report.Result step)
    , result ∷ Maybe o
    , report ∷ Report.Report step
    }
run (Form { layout, reporter }) input = do
  result ← runReporter reporter input
  let
    report = Object.Builder.build (Polyform.Reporter.report result)
  let
    layout' = layout <#> \k → case Object.lookup k report of
      -- | TODO: We should probably handle this as a form DSL error
      -- |       because all results should be present in the result
      -- |       object.
      Nothing → { input: Nothing, result: Nothing }
      Just r → r
  pure $ { layout: layout', result: hush <<< Polyform.Reporter.toEither $ result, report: report }

emptyLayout ∷ ∀ layout m i o step. Functor layout ⇒ Form m (layout Report.Key) step i o → layout (Report.Result step)
emptyLayout = \(Form { layout }) →
  let
    n = { input: Nothing, result: Nothing }
  in
    layout <#> const n

-- | Provided to simplify inference
emptyLayout' ∷ ∀ layout m i o step. Functor layout ⇒ Monad m ⇒ Form m (layout Report.Key) step i o → m (layout (Report.Result step))
emptyLayout' = \(Form { layout }) →
  let
    n = { input: Nothing, result: Nothing }
  in
    pure (layout <#> const n)

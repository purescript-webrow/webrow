module WebRow.Forms.Bi.Form where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Polyform (Dual(..))
import Polyform.Reporter.Dual (Dual) as Reporter
import Polyform.Reporter.Dual (runReporter, runSerializer) as Reporter.Dual
import Polyform.Reporter.R (R(..)) as R
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..))
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (UrlDecoded)

newtype Form m layout o = Form
  { dual ∷ Reporter.Dual m layout UrlDecoded o
  , default ∷ m layout
  }

build ∷ ∀ m o widget. Builder m widget UrlDecoded o → Form m widget o
build (Builder (BuilderD b)) =
  let
    { dualD, default } = BuilderM.eval b
  in
    Form { dual: Dual dualD, default }

default
  ∷ ∀ m msg o widget. Form m (Layout msg widget) o
  → m (Layout msg widget)
default (Form { default: d }) = d

serialize
  ∷ ∀ m msg o widget
  . Applicative m
  ⇒ Form m (Layout msg widget) o
  → o
  → m (Tuple UrlDecoded (Layout msg widget))
serialize (Form { dual }) o = Reporter.Dual.runSerializer dual o

validate
  ∷ ∀ m msg o widget
  . Monad m
  ⇒ Form m (Layout msg widget) o
  → UrlDecoded
  → m (Tuple (Layout msg widget) (Maybe o))
validate (Form { dual }) input = Reporter.Dual.runReporter dual input >>= case _ of
  R.Success r o → pure $ Tuple r (Just o)
  R.Failure r → pure $ Tuple r Nothing

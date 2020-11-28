module WebRow.Forms.Bi.Form where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Polyform (Dual(..))
import Polyform.Reporter.Dual (Dual) as Reporter
import Polyform.Reporter.Dual (runReporter, runSerializer) as Reporter.Dual
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..))
import WebRow.Forms.Bi.Builder (Default) as Builder
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (UrlDecoded)

-- | `m` in the context of `default` seems a bit
-- | to restrictive but I'm not sure how to
-- | limit here the build up process to something
-- | like tranlsations / localizations etc.
newtype Form m n layout o
  = Form
  { dual ∷ Reporter.Dual m layout UrlDecoded o
  , default ∷ n (Builder.Default layout)
  }

build ∷ ∀ m n o widget. Builder m n widget UrlDecoded o → Form m n widget o
build (Builder (BuilderD b)) =
  let
    { dualD, default } = BuilderM.eval b
  in
    Form { dual: Dual dualD, default }

default ∷
  ∀ m msg n o widget.
  Form m n (Layout msg widget) o →
  n (Builder.Default (Layout msg widget))
default (Form { default: d }) = d

serialize ∷
  ∀ m msg n o widget.
  Applicative m ⇒
  Form m n (Layout msg widget) o →
  o →
  m (Tuple UrlDecoded (Layout msg widget))
serialize (Form { dual }) o = Reporter.Dual.runSerializer dual o

validate ∷
  ∀ m msg n o widget.
  Monad m ⇒
  Form m n (Layout msg widget) o →
  UrlDecoded →
  m (Tuple (Maybe o) (Layout msg widget))
validate (Form { dual }) input = Reporter.Dual.runReporter dual input

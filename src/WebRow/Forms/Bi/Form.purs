module WebRow.Forms.Bi.Form where

import Prelude

import Control.Monad.Writer (WriterT, mapWriter, mapWriterT, runWriterT)
import Data.Lens (over) as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong (first, second)
import Data.Tuple.Nested (type (/\))
import Polyform (Dual(..))
import Polyform.Batteries.UrlEncoded (Query) as UrlEncoded
import Polyform.Dual (Dual, hoistParser, parser, serializer) as Dual
import Polyform.Dual (hoistSerializer)
import Polyform.Reporter (Reporter, lmapReporter)
import Polyform.Reporter (runReporter) as Reporter
import Type.Prelude (SProxy(..))
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
  { dual ∷ Dual.Dual (Reporter m layout) (WriterT layout n) UrlDecoded o
  , default ∷ n (Builder.Default layout)
  }

-- | Should we change the order so layout is on the last position?
-- | and we get a proper Functor instance for Form?
mapLayout ∷ ∀ layout layout' m n o. Functor n ⇒ Monad m ⇒ (layout → layout') → Form m n layout o → Form m n layout' o
mapLayout f (Form r) = Form
  { dual: hoistSerializer (mapWriterT (map (second f))) <<< Dual.hoistParser (lmapReporter f) $ r.dual
  , default: map (Lens.over (prop (SProxy ∷ SProxy "layout")) f) r.default
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
  n (UrlEncoded.Query /\ Layout msg widget)
serialize (Form { dual }) = map runWriterT <<< Dual.serializer $ dual

validate ∷
  ∀ m msg n o widget.
  Monad m ⇒
  Form m n (Layout msg widget) o →
  UrlDecoded →
  m (Maybe o /\ Layout msg widget)
validate (Form { dual }) = Reporter.runReporter (Dual.parser dual)

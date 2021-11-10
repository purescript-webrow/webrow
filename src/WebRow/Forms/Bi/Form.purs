module WebRow.Forms.Bi.Form where

import Prelude

import Control.Monad.Writer (mapWriterT)
import Data.Lens (over) as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong (second)
import Data.Tuple.Nested (type (/\))
import Polyform (Dual(..))
import Polyform.Batteries.UrlEncoded (Query) as UrlEncoded
import Polyform.Dual (hoistParser, parser) as Dual
import Polyform.Dual (hoistSerializer)
import Polyform.Reporter (lmapReporter)
import Polyform.Reporter (runReporter) as Reporter
import Polyform.Reporter.Dual (Dual, hoist, runSerializer) as Reporter.Dual
import Type.Prelude (SProxy(..))
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..))
import WebRow.Forms.Bi.Builder (Default) as Builder
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Payload (UrlDecoded)

-- | `m` in the context of `default` seems a bit
-- | to restrictive but I'm not sure how to
-- | limit here the build up process to something
-- | like tranlsations / localizations etc.
newtype Form m layout o
  = Form
  { dual ∷ Reporter.Dual.Dual m layout UrlDecoded o
  , default ∷ Builder.Default layout
  }

-- | Should we change the order so layout is on the last position?
-- | and we get a proper Functor instance for Form?
-- | We are not able to provide any interesting instance for `o` because
-- | it is part of the internal `Dual`.
mapLayout ∷ ∀ layout layout' m o. Monad m ⇒ (layout → layout') → Form m layout o → Form m layout' o
mapLayout f (Form r) = Form
  { dual: hoistSerializer (mapWriterT (map (second f))) <<< Dual.hoistParser (lmapReporter f) $ r.dual
  , default:
      Lens.over (prop (SProxy ∷ SProxy "overwrite")) (map f)
      $ Lens.over (prop (SProxy ∷ SProxy "layout")) f r.default
  }

build ∷ ∀ m o widget. Builder m widget UrlDecoded o → Form m widget o
build (Builder (BuilderD b)) =
  let
    { dualD, default } = BuilderM.eval b
  in
    Form { dual: Dual dualD, default }

hoist ∷
  ∀ layout m m' o.
  Functor m ⇒
  (m ~> m') →
  Form m layout o →
  Form m' layout o
hoist f (Form { default: d, dual }) = Form { default: d, dual: Reporter.Dual.hoist f dual }

default ∷
  ∀ layout m o.
  Form m layout o →
  Builder.Default layout
default (Form { default: d }) = d

serialize ∷
  ∀ layout m o.
  Form m layout o →
  o →
  UrlEncoded.Query /\ layout
serialize (Form { dual }) = Reporter.Dual.runSerializer $ dual

validate ∷
  ∀ layout m o.
  Monad m ⇒
  Form m layout o →
  UrlDecoded →
  m (Maybe o /\ layout)
validate (Form { dual }) = Reporter.runReporter (Dual.parser dual)


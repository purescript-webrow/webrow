module WebRow.Forms.Widget where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, for)
import Data.Variant (Variant)
import WebRow.Forms.BuilderM (BuilderM)
import WebRow.Forms.BuilderM (id) as BuilderM
import WebRow.Forms.Payload (Key, UrlDecoded, Value, lookup) as Payload

type Payload inputs = inputs (Maybe Payload.Value)

type Names inputs = inputs Payload.Key

type Widget widgets = Variant widgets

type Initials inputs o =
  { payload ∷ Payload inputs
  , names ∷ Names inputs
  , result ∷ Maybe (Either (Array String) o)
  }

type Constructor m inputs widgets o =
  Initials inputs o → m (Widget widgets)

names
  ∷ ∀ inputs
  . Monoid (inputs Unit)
  ⇒ Traversable inputs
  ⇒ BuilderM (Names inputs)
names = for (mempty ∷ inputs Unit) (\_ → BuilderM.id)

payload
  ∷ ∀ inputs
  . Functor inputs
  ⇒ inputs Payload.Key
  → Payload.UrlDecoded
  → Payload inputs
payload inputs urlDecoded = map (flip Payload.lookup urlDecoded) inputs


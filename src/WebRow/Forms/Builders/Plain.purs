module WebRow.Forms.Builders.Plain where

import Prelude

import Data.Either (Either(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Data.Variant (Variant, inj)
import Global.Unsafe (unsafeStringify)
import Polyform.Reporter (R(..), hoistFnMR)
import Polyform.Reporter (Reporter) as Polyform.Reporter
import Polyform.Reporter (hoistValidatorWith) as Reporter
import Polyform.Validator (Validator(..)) as Polyform.Validator
import Polyform.Validator (hoistFn) as Validator
import Polyform.Validator (lmapValidator, runValidator)
import Polyform.Validators.UrlEncoded (FieldValueValidator)
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Forms.Layout (Layout(..), closeSection, sectionReporter) as Layout
import WebRow.Forms.Payload (Key) as Payload
import WebRow.Forms.Payload (Value, UrlDecoded)
import WebRow.Forms.Plain (Form(..)) as Plain
import WebRow.Forms.Validation.Report (Result, Key) as Report
import WebRow.Forms.Validation.Report (Result, Key) as Validation.Report
import WebRow.Forms.Validation.Report (Result, Report) as Validation.Report
import WebRow.Utils.Foreign.Object.Builder (Builder, insert) as Foreign.Object.Builder

-- | Let's test this architecture in flatten monomoprhic 
-- | validation step result mode.
type ReprRow = (repr ∷ String)

type Repr = Variant ReprRow

type Report = Validation.Report.Report Repr

type Step = Foreign.Object.Builder.Builder (Validation.Report.Result Repr)

type Reporter m i o = Polyform.Reporter.Reporter m Step i o

data Field
  = InputField { name ∷ String, type_ ∷ String }

type Layout = Layout.Layout Field Payload.Key Report.Key

type Form m i o = Plain.Form m Layout Repr i o

hoistValidator name = Reporter.hoistValidatorWith
  (toStep <<< Left)
  (toStep <<< Right <<< (inj (SProxy ∷ SProxy "repr") <<< unsafeStringify))
  where
    toStep result = Foreign.Object.Builder.insert name result

field
  ∷ ∀ m o
  . Monad m
  ⇒ { name ∷ String, type_ ∷ String }
  → FieldValueValidator m o
  → Form m UrlDecoded o
field l@{ name, type_ } fieldValidator = Plain.Form { layout, reporter }
  where
    -- | TODO: Replace `name` based `Key` with
    -- | a proper id generation based on name and
    -- | some internal sequence.
    key = name
    reporter = hoistValidator
      key
      (Validator.hoistFn (Map.lookup key) >>> fieldValidator)

    layout = Layout.Field { input: key, field: InputField l, result: key }

-- | TODO: Just for debugging
passwordField ∷ ∀ o m. Monad m ⇒ String → FieldValueValidator m o → Form m UrlDecoded o
passwordField name = field { name, type_: "password" }

closeSection ∷ ∀ i m o. String → Form m i o → Form m i o
closeSection title (Plain.Form { layout, reporter }) = Plain.Form
  { layout: Layout.closeSection title layout, reporter }

sectionValidator ∷ ∀ i m o. Functor m ⇒ String → Polyform.Validator.Validator m (Array String) i o → Form m i o
sectionValidator name validator =
  Plain.Form { layout: Layout.sectionReporter name, reporter }
  where
    reporter = hoistValidator name validator


module WebRow.Forms.Bi
  ( Bi
  , Builder
  , FieldDual
  , TextInputInitials
  , TextInputInitialsBase
  , closeSection
  , module Exports
  , fieldBuilder
  , passwordInputBuilder
  , sectionDual
  , textInputBuilder
  ) where

-- | This module provides predefined field constructors and
-- | aliases which use `Forms.Layout` for bidirectional forms.
-- |
-- | If you don't like `Forms.Layout` there is a layout
-- | agnostic definition of `Builder` and `Form` itself provided in
-- | the submodules.
import Prelude
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List (catMaybes, fromFoldable, zip) as List
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce) as Closed
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem.Closed
import Data.Variant (Variant)
import Polyform (Dual(..)) as Polyform
import Polyform.Batteries (Errors)
import Polyform.Batteries.UrlEncoded.Duals (value) as Batteries
import Polyform.Batteries.UrlEncoded.Query (Decoded(..)) as Query
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Dual (Dual(..), dual) as Dual
import Polyform.Reporter (liftFn) as Polyform.Reporter
import Polyform.Reporter.Dual (Dual, DualD) as Reporter
import Polyform.Reporter.Dual (liftValidatorDualWith, liftValidatorDualWithM, lmapM) as Reporter.Dual
import Polyform.Validator.Dual (Dual) as Validator
import Polyform.Validator.Dual (iso, lmapM) as Validator.Dual
import Type.Row (type (+))
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..)) as B
import WebRow.Forms.Bi.Form (Form, default, serialize, validate) as Form
import WebRow.Forms.BuilderM (BuilderM)
import WebRow.Forms.Layout (LayoutBase(..), closeSection, sectionErrors) as Layout
import WebRow.Forms.Payload (Key, Value) as Payload
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Uni (Layout, MessageM, PasswordInputInitials)
import WebRow.Forms.Uni (Layout, MessageM, PasswordInputInitials) as Exports
import WebRow.Forms.Widget (Constructor, Payload, Names, names, payload) as Widget
import WebRow.Forms.Widgets (TextInput)
import WebRow.Forms.Widgets (textInput) as Widgets
import WebRow.Message (message)

-- | Basic building block for a widget validation is a validator
-- | which has predefined shape for its input:
-- |
-- | * Single field widgets take need single input value from the data
-- |  which was given in a form / request. So we pass here `Identity`
-- |  container.
-- |
-- | * Multi field widgets could possibly have here a homogenous `Record`
-- |  which is filled by the framework with the data from general payload.
-- |  Framework is going to also fill the input structure with the names
-- |  which should be used for every form input during rendering to close
-- |  the data flow loop correctly.
type FieldDual eff info i o
  = Validator.Dual (MessageM info eff) (Errors info) i o

type Dual eff info widgets i o
  = Reporter.Dual (MessageM info eff) (Layout widgets) i o

newtype Builder eff info widgets i o
  = Builder
  ( B.BuilderD
      (MessageM info eff)
      (Layout widgets)
      i
      o
      o
  )

newtype Bi eff info widgets o
  = Bi (Form.Form (MessageM info eff) (Layout widgets) o)

instance semigroupoidBuilder ∷ Semigroupoid (Builder eff info widgets) where
  compose (Builder bd1) (Builder bd2) =
    let
      B.Builder bd = compose (B.Builder bd1) (B.Builder bd2)
    in
      Builder bd

instance categoryBuilder ∷ Category (Builder eff info widgets) where
  identity =
    let
      B.Builder bdi = identity
    in
      Builder bdi

-- | Takes:
-- |
-- |  * A "field constructor" which requires possible validation result.
-- |
-- |  * A `Dual` which works on a structured input with query values
-- |  (so it doesn't care about field "names" in the HTML form).
-- |
-- | Returns a form `Builder` which can be composed into larger one easily.
fieldBuilder ∷
  ∀ eff info inputs widgets o.
  Monoid (inputs Unit) ⇒
  Traversable inputs ⇒
  { constructor ∷ Widget.Constructor (MessageM info eff) inputs widgets o
  , defaults ∷ Widget.Payload inputs
  , dual ∷ FieldDual eff info (Widget.Payload inputs) o
  } →
  Builder eff info widgets UrlDecoded o
fieldBuilder { constructor, defaults, dual } =
  builder do
    ns ∷ Widget.Names inputs ← Widget.names
    let
      constructor' = map Layout.Widget <<< constructor

      fromSuccess ∷ Tuple (Widget.Payload inputs) o → MessageM info eff (Layout widgets)
      fromSuccess (Tuple payload o) = constructor' { payload, names: ns, result: Just (Right o) }

      fromFailure ∷ Tuple (Widget.Payload inputs) (Errors info) → MessageM info eff (Layout widgets)
      fromFailure (Tuple payload e) = do
        -- | We are doing error rendering here
        e' ← traverse message e
        constructor' { payload, names: ns, result: Just (Left e') }

      widgetDual ∷ Dual eff info widgets (Widget.Payload inputs) o
      widgetDual = Reporter.Dual.liftValidatorDualWithM fromFailure fromSuccess dual

      dropMissing ∷ List (Tuple Payload.Key (Maybe Payload.Value)) → List (Tuple Payload.Key Payload.Value)
      dropMissing = List.catMaybes <<< map sequence

      payloadDual ∷ Dual eff info widgets UrlDecoded (Widget.Payload inputs)
      payloadDual =
        Dual.dual
          (Polyform.Reporter.liftFn (Widget.payload ns))
          ( pure
              <<< Query.Decoded
              <<< Map.fromFoldable
              <<< dropMissing
              <<< List.zip (List.fromFoldable ns)
              <<< List.fromFoldable
          )
    pure
      { dualD: un Dual.Dual $ widgetDual <<< payloadDual
      , default: constructor' { payload: defaults, names: ns, result: Nothing }
      }

builder ∷
  ∀ eff info i o widgets.
  BuilderM
    { default ∷ MessageM info eff (Layout widgets)
    , dualD ∷ Reporter.DualD (MessageM info eff) (Layout widgets) i o o
    } →
  Builder eff info widgets i o
builder = Builder <<< B.BuilderD

-- | We reuse this row in Forms.Bi
type TextInputInitialsBase (r ∷ # Type)
  = ( default ∷ Opt String
    , label ∷ Opt String
    , type_ ∷ Opt String
    , placeholder ∷ Opt String
    , helpText ∷ Opt String
    | r
    )

type TextInputInitials info eff o
  = {
    | TextInputInitialsBase
      + ( dual ∷ FieldDual eff info (Maybe Payload.Value) o )
    }

textInputBuilder ∷
  ∀ args eff info o r.
  NoProblem.Closed.Coerce args (TextInputInitials info eff o) ⇒
  args →
  Builder eff info (TextInput + r) UrlDecoded o
textInputBuilder args =
  fieldBuilder
    { constructor, defaults: Identity (Just [ default ! "" ]), dual: dual' }
  where
  i@{ default, dual } = NoProblem.Closed.coerce args ∷ TextInputInitials info eff o

  dual' = dual <<< Validator.Dual.iso (un Identity) Identity

  constructor { payload: Identity payload, names: Identity name, result } =
    pure
      $ Widgets.textInput
          { type_: i.type_ ! "text"
          , helpText: i.helpText # NoProblem.toMaybe
          , label: i.label # NoProblem.toMaybe
          , payload
          , placeholder: i.placeholder # NoProblem.toMaybe
          , name
          , result: map (mkExists <<< Identity) <$> result
          }

passwordInputBuilder ∷
  ∀ args eff info r.
  Closed.Coerce args (PasswordInputInitials eff info) ⇒
  args →
  Builder
    eff
    (MissingValue + info)
    (TextInput + r)
    UrlDecoded
    String
passwordInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , dual: Batteries.value
    , type_: "password"
    }
  where
  i@{ helpText, label, placeholder } = NoProblem.Closed.coerce args ∷ (PasswordInputInitials eff info)

closeSection ∷ ∀ eff i info o widgets. Variant info → Builder eff info widgets i o → Builder eff info widgets i o
closeSection title (Builder (B.BuilderD bd)) =
  builder do
    { default: d, dualD } ← bd
    pure
      { default: d >>= close
      , dualD: un Polyform.Dual (Reporter.Dual.lmapM close (Polyform.Dual dualD))
      }
  where
  close s = do
    t ← message title
    pure $ Layout.closeSection t s

sectionDual ∷
  ∀ eff i info o widgets.
  FieldDual eff info i o →
  Builder eff info widgets i o
sectionDual dual = builder $ pure { default: pure mempty, dualD }
  where
  dual' = Validator.Dual.lmapM (traverse message) dual

  Polyform.Dual dualD = Reporter.Dual.liftValidatorDualWith (Tuple.snd >>> Layout.sectionErrors) (const mempty) dual'

default ∷
  ∀ eff info o widgets.
  Bi eff info widgets o →
  MessageM info eff (Layout widgets)
default (Bi form) = Form.default form

serialize ∷
  ∀ eff info o widgets.
  Bi eff info widgets o →
  o →
  MessageM info eff (Tuple UrlDecoded (Layout widgets))
serialize (Bi form) = Form.serialize form

validate ∷
  ∀ eff info o widgets.
  Bi eff info widgets o →
  UrlDecoded →
  MessageM info eff (Tuple (Maybe o) (Layout widgets))
validate (Bi form) = Form.validate form

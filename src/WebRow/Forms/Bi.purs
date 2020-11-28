module WebRow.Forms.Bi
  ( Bi
  , Builder(..)
  , build
  , default
  , diverge
  , dual
  , FieldDual
  , TextInputInitials
  , TextInputInitialsBase
  , closeSection
  , fieldBuilder
  , passwordInputBuilder
  , sectionDual
  , serialize
  , textInputBuilder
  , validate
  , (~)
  ) where

-- | This module provides predefined field constructors and
-- | aliases which use `Forms.Layout` for bidirectional forms.
-- |
-- | If you don't like `Forms.Layout` there is a layout
-- | agnostic definition of `Builder` and `Form` itself provided in
-- | the submodules.

import Prelude

import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List (catMaybes, fromFoldable, zip) as List
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Profunctor (lcmap)
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
import Polyform.Batteries.UrlEncoded (Query(..)) as UrlEncoded
import Polyform.Batteries.UrlEncoded.Duals (value) as Batteries
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Dual (Dual(..), dual) as Dual
import Polyform.Reporter (liftFn) as Polyform.Reporter
import Polyform.Reporter.Dual (DualD, Dual) as Reporter
import Polyform.Reporter.Dual (liftValidatorDualWith, liftValidatorDualWithM, lmapM) as Reporter.Dual
import Polyform.Validator.Dual (Dual) as Validator
import Polyform.Validator.Dual (iso, lmapM) as Validator.Dual
import Run (expand) as Run
import Type.Row (type (+))
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..), Default) as B
import WebRow.Forms.Bi.Builder (Default) as Builder
import WebRow.Forms.Bi.Form (Form(..), default, serialize, validate) as Form
import WebRow.Forms.BuilderM (BuilderM)
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (LayoutBase(..), closeSection, sectionErrors) as Layout
import WebRow.Forms.Payload (Key, Value) as Payload
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Uni (Layout, MessageM, PasswordInputInitials)
import WebRow.Forms.Widget (Constructor, Names, Payload, dump, names, payload) as Widget
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
      (MessageM info ())
      (Layout widgets)
      i
      o
      o
  )

derive newtype instance functorBuilder ∷ Functor (Builder eff info widgets i)

derive newtype instance applyBuilder ∷ Semigroup i ⇒ Apply (Builder eff info widgets i)

newtype Bi eff info widgets o
  = Bi (Form.Form (MessageM info eff) (MessageM info ()) (Layout widgets) o)

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

infixl 5 diverge as ~

diverge ∷
  ∀ eff i info o o' widgets.
  (o' → o) →
  Builder eff info widgets i o →
  B.BuilderD (MessageM info eff) (MessageM info ()) (Layout widgets) i o' o
diverge f (Builder b) = lcmap f b

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
  { constructor ∷ Widget.Constructor (MessageM info ()) inputs widgets o
  , defaults ∷ Widget.Payload inputs
  , dual ∷ FieldDual eff info (Widget.Payload inputs) o
  } →
  Builder eff info widgets UrlDecoded o
fieldBuilder { constructor, defaults, dual: d } =
  builder do
    ns ∷ Widget.Names inputs ← Widget.names
    let
      constructor' = map Layout.Widget <<< constructor

      fromSuccess ∷ Tuple (Widget.Payload inputs) o → MessageM info eff (Layout widgets)
      fromSuccess (Tuple payload o) = Run.expand $ constructor' { payload, names: ns, result: Just (Right o) }

      fromFailure ∷ Tuple (Widget.Payload inputs) (Errors info) → MessageM info eff (Layout widgets)
      fromFailure (Tuple payload e) = Run.expand $ do
        -- | We are doing error rendering here
        e' ← traverse message e
        constructor' { payload, names: ns, result: Just (Left e') }

      widgetDual ∷ Dual eff info widgets (Widget.Payload inputs) o
      widgetDual = Reporter.Dual.liftValidatorDualWithM fromFailure fromSuccess d

      dropMissing ∷ List (Tuple Payload.Key (Maybe Payload.Value)) → List (Tuple Payload.Key Payload.Value)
      dropMissing = List.catMaybes <<< map sequence

      payloadDual ∷ Dual eff info widgets UrlDecoded (Widget.Payload inputs)
      payloadDual =
        Dual.dual
          (Polyform.Reporter.liftFn (Widget.payload ns))
          ( pure
              <<< UrlEncoded.Query
              <<< Map.fromFoldable
              <<< dropMissing
              <<< List.zip (List.fromFoldable ns)
              <<< List.fromFoldable
          )
    pure
      { dualD: un Dual.Dual $ widgetDual <<< payloadDual
      , default:
        do
          layout ← constructor' { payload: defaults, names: ns, result: Nothing }
          pure { layout, payload: Widget.dump ns defaults }
      }

builder ∷
  ∀ eff info i o widgets.
  BuilderM
    { default ∷ MessageM info () (Builder.Default (Layout widgets))
    , dualD ∷ Reporter.DualD (MessageM info eff) (Layout widgets) i o o
    } →
  Builder eff info widgets i o
builder = Builder <<< B.BuilderD

type TextInputInitialsBase info (r ∷ #Type)
  = ( helpText ∷ Opt (Variant info)
    , default ∷ Opt String
    , label ∷ Opt (Variant info)
    , type_ ∷ Opt String
    , placeholder ∷ Opt (Variant info)
    | r
    )

type TextInputInitials info eff o
  = {
    | TextInputInitialsBase info
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

  constructor { payload: Identity payload, names: Identity name, result } = do
    helpText ← i.helpText # NoProblem.toMaybe # traverse message
    label ← i.label # NoProblem.toMaybe # traverse message
    placeholder ← i.placeholder # NoProblem.toMaybe # traverse message
    pure
      $ Widgets.textInput
          { type_: i.type_ ! "text"
          , helpText
          , label
          , payload
          , placeholder
          , name
          , result: either Just (const Nothing) <$> result
          }

passwordInputBuilder ∷
  ∀ args eff info r.
  Closed.Coerce args (PasswordInputInitials eff (MissingValue + info)) ⇒
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
  i@{ helpText, label, placeholder } = NoProblem.Closed.coerce args ∷ (PasswordInputInitials eff (MissingValue + info))

closeSection ∷ ∀ eff i info o widgets. Variant info → Builder eff info widgets i o → Builder eff info widgets i o
closeSection title (Builder (B.BuilderD bd)) =
  builder do
    { default: d, dualD } ← bd
    pure
      { default:
        d
          >>= \{ layout, payload } → do
              layout' ← close' layout
              pure { layout: layout', payload }
      , dualD: un Polyform.Dual (Reporter.Dual.lmapM close (Polyform.Dual dualD))
      }
  where
  close' s = do
    t ← message title
    pure $ Layout.closeSection t s
  close s = do
    t ← message title
    pure $ Layout.closeSection t s

sectionDual ∷
  ∀ eff i info o widgets.
  FieldDual eff info i o →
  Builder eff info widgets i o
sectionDual d = builder $ pure { default: pure mempty, dualD }
  where
  d' = Validator.Dual.lmapM (traverse message) d

  Polyform.Dual dualD = Reporter.Dual.liftValidatorDualWith (Tuple.snd >>> Layout.sectionErrors) (const mempty) d'

default ∷
  ∀ eff info o widgets.
  Bi eff info widgets o →
  MessageM info () (B.Default (Layout widgets))
default (Bi form) = Form.default form

-- | Make this consistnent with the above
-- | probably we want to use just `Tuple` everywhere.
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

build ∷
  ∀ eff info o widgets.
  Builder eff info widgets UrlDecoded o →
  Bi eff info widgets o
build (Builder (B.BuilderD b)) = Bi $ Form.Form $ (\r@{ dualD } → { default: r.default, dual: Polyform.Dual r.dualD }) <<< BuilderM.eval $ b

dual ∷
  ∀ eff info o widgets.
  Bi eff info widgets o →
  Dual eff info widgets UrlDecoded o
dual (Bi (Form.Form form)) = form.dual

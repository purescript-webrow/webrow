module WebRow.Forms.Bi
  ( Bi
  , Builder(..)
  , build
  , builder
  , closeSection
  , default
  , diverge
  , dual
  , fieldBuilder
  , FieldDual
  , fromDual
  , passwordInputBuilder
  , sectionDual
  , serialize
  , textInputBuilder
  , TextInputInitials
  , TextInputInitialsBase
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
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce) as Closed
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem.Closed
import Polyform (Dual(..)) as Polyform
import Polyform.Batteries (Msg)
import Polyform.Batteries.UrlEncoded (Query(..)) as UrlEncoded
import Polyform.Batteries.UrlEncoded.Duals (value) as Batteries
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Dual (Dual(..), dual) as Dual
import Polyform.Reporter (liftFn) as Polyform.Reporter
import Polyform.Reporter.Dual (DualD, Dual) as Reporter
import Polyform.Reporter.Dual (liftValidatorDualWith, liftValidatorDualWithM, lmapM) as Reporter.Dual
import Polyform.Validator.Dual (Dual) as Validator
import Polyform.Validator.Dual (iso) as Validator.Dual
import Type.Row (type (+))
import WebRow.Forms.Bi.Builder (Builder(..), BuilderD(..), Default, fromDual) as B
import WebRow.Forms.Bi.Builder (Default) as Builder
import WebRow.Forms.Bi.Form (Form(..), default, serialize, validate) as Form
import WebRow.Forms.BuilderM (BuilderM)
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Layout (LayoutBase(..), closeSection, sectionErrors) as Layout
import WebRow.Forms.Payload (Key, Value) as Payload
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Uni (PasswordInputInitials)
import WebRow.Forms.Widget (Constructor, Payload, dump, names, payload) as Widget
import WebRow.Forms.Widgets (TextInput)
import WebRow.Forms.Widgets (textInput) as Widgets

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
type FieldDual m msg i o
  = Validator.Dual m (Array msg) i o

type Dual m msg widget i o
  = Reporter.Dual m (Layout msg widget) i o

-- | TODO: Drop monads unification.
newtype Builder m msg widget i o
  = Builder
  ( B.BuilderD
      m
      m
      (Layout msg widget)
      i
      o
      o
  )

derive newtype instance functorBuilder ∷ Monad m ⇒ Functor (Builder m msg widget i)

derive newtype instance applyBuilder ∷ (Semigroup i, Monad m) ⇒ Apply (Builder m msg widgets i)

newtype Bi m n msg widgets o
  = Bi (Form.Form m n (Layout msg widgets) o)

instance semigroupoidBuilder ∷ Monad m ⇒ Semigroupoid (Builder m msg widget) where
  compose (Builder bd1) (Builder bd2) =
    let
      B.Builder bd = compose (B.Builder bd1) (B.Builder bd2)
    in
      Builder bd

instance categoryBuilder ∷ Monad m ⇒ Category (Builder m msg widget) where
  identity =
    let
      B.Builder bdi = identity
    in
      Builder bdi

fromDual ∷ ∀ i m msg o widget. Monad m ⇒ Dual m msg widget i o → Builder m msg widget i o
fromDual d =
  let
    B.Builder bd = B.fromDual d
  in
    Builder bd

infixl 5 diverge as ~

diverge ∷
  ∀ i m msg o o' widget.
  Functor m ⇒
  (o' → o) →
  Builder m msg widget i o →
  B.BuilderD m m (Layout msg widget) i o' o
diverge f (Builder b) = lcmap f b

-- | Takes:
-- |
-- |  * A "widget constructor" which requires a possible validation result.
-- |
-- |  * A `Dual` which works on a structured input with query values
-- |  (so it doesn't care about field "names" in the HTML form).
-- |
-- | Returns a form `Builder` which can be composed into larger one easily.
widgetBuilder ∷
  ∀ inputs m msg widget o.
  Monad m ⇒
  Monoid (inputs Unit) ⇒
  Traversable inputs ⇒
  { constructor ∷ Widget.Constructor m msg inputs widget o
  , defaults ∷ Widget.Payload inputs
  , dual ∷ FieldDual m msg (Widget.Payload inputs) o
  , widgetId ∷ Maybe String
  } →
  Builder m msg widget UrlDecoded o
widgetBuilder { constructor, defaults, dual: d, widgetId } =
  builder do
    ns ← Widget.names
    let
      constructor' = map step <<< constructor
        where
        step widget = Layout.Widget { id: widgetId, widget }

      fromSuccess ∷ Tuple (Widget.Payload inputs) o → m (Layout msg widget)
      fromSuccess (Tuple payload o) = constructor' { payload, names: ns, result: Just (Right o) }

      fromFailure ∷ Tuple (Widget.Payload inputs) (Array msg) → m (Layout msg widget)
      fromFailure (Tuple payload e) = constructor' { payload, names: ns, result: Just (Left e) }

      widgetDual ∷ Dual m msg widget (Widget.Payload inputs) o
      widgetDual = Reporter.Dual.liftValidatorDualWithM fromFailure fromSuccess d

      dropMissing ∷ List (Tuple Payload.Key (Maybe Payload.Value)) → List (Tuple Payload.Key Payload.Value)
      dropMissing = List.catMaybes <<< map sequence

      payloadDual ∷ Dual m msg widget UrlDecoded (Widget.Payload inputs)
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

-- | widgetBuilder ∷
-- |   ∀ eff info inputs widgets o.
-- |   Monoid (inputs Unit) ⇒
-- |   Traversable inputs ⇒
-- |   { constructor ∷ Widget.Constructor (MessageM info ()) inputs widgets o
-- |   , defaults ∷ Widget.Payload inputs
-- |   , dual ∷ FieldDual eff info (Widget.Payload inputs) o
-- |   } →
-- |   Builder eff info widgets UrlDecoded o
type Id a
  = a

-- | Simple widget which contains only a single "field".
-- | Which means that it contains single payload "name" and
-- | possibly its value.
-- | This helper wraps this stuff in `Identity` to fullfill
-- | more generic widget builder API where `Traversable inputs`
-- | is expected.
fieldBuilder ∷
  ∀ m msg widget o.
  Monad m ⇒
  { constructor ∷ Widget.Constructor m msg Id widget o
  , default ∷ Maybe Payload.Value
  , dual ∷ FieldDual m msg (Maybe Payload.Value) o
  , widgetId ∷ Maybe String
  } →
  Builder m msg widget UrlDecoded o
fieldBuilder { constructor, default: def, dual: d, widgetId } =
  widgetBuilder
    { constructor: constructor'
    , defaults: Identity def
    , dual: d'
    , widgetId
    }
  where
  constructor' { names: Identity names, payload: Identity payload, result } = constructor { payload, names, result }

  d' = d <<< Validator.Dual.iso (un Identity) Identity

builder ∷
  ∀ i m msg o widget.
  BuilderM
    { default ∷ m (Builder.Default (Layout msg widget))
    , dualD ∷ Reporter.DualD m (Layout msg widget) i o o
    } →
  Builder m msg widget i o
builder = Builder <<< B.BuilderD

type TextInputInitialsBase msg (r ∷ #Type)
  = ( helpText ∷ Opt msg
    , default ∷ Opt String
    , id ∷ Opt String
    , label ∷ Opt msg
    , type_ ∷ Opt String
    , placeholder ∷ Opt msg
    , widgetId ∷ Opt String
    | r
    )

type TextInputInitials msg m o
  = { 
    | TextInputInitialsBase msg
      + ( dual ∷ FieldDual m msg (Maybe Payload.Value) o )
    }

textInputBuilder ∷
  ∀ args m msg o r.
  Monad m ⇒
  NoProblem.Closed.Coerce args (TextInputInitials msg m o) ⇒
  args →
  Builder m msg (TextInput msg () + r) UrlDecoded o
textInputBuilder args =
  fieldBuilder
    { constructor
    , default: Just [ default ! "" ]
    , dual
    , widgetId: NoProblem.toMaybe i.widgetId
    }
  where
  i@{ default, dual } = NoProblem.Closed.coerce args ∷ TextInputInitials msg m o

  constructor { payload, names: name, result } = do
    let
      helpText = NoProblem.toMaybe i.helpText

      label = NoProblem.toMaybe i.label

      placeholder = NoProblem.toMaybe i.placeholder
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

-- optTextInputBuilder ∷
--   ∀ args eff info r.
--   NoProblem.Closed.Coerce args (TextInputInitials eff info o) ⇒
--   args →
--   Builder
--     eff
--     info
--     (TextInput + r)
--     UrlDecoded
--     (Maybe o)
-- optTextInputBuilder args =
--   textInputBuilder
--     { placeholder
--     , helpText
--     , label
--     , name
--     , dual
--     }
--   where
--   dual = Dual.dual
--     (liftFn Array.head)
--     (map Array.singleton >>> pure)
--   i@{ helpText, label, name, placeholder } = NoProblem.Closed.coerce args ∷ PasswordInputInitials eff info
passwordInputBuilder ∷
  ∀ args m msg r.
  Closed.Coerce args (PasswordInputInitials m (Msg (MissingValue + msg))) ⇒
  Monad m ⇒
  args →
  Builder
    m
    (Msg (MissingValue + msg))
    (TextInput (Msg (MissingValue + msg)) () + r)
    UrlDecoded
    String
passwordInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , dual: Batteries.value ∷ FieldDual m (Msg (MissingValue + msg)) (Maybe Payload.Value) String
    , type_: "password"
    }
  where
  i@{ helpText, label, placeholder } = NoProblem.Closed.coerce args ∷ (PasswordInputInitials m (Msg (MissingValue + msg)))

-- | Move this to internals
type LayoutHeader' msg
  = { id ∷ Opt String, title ∷ Opt msg }

closeSection ∷
  ∀ args i m msg o widget.
  Monad m ⇒
  NoProblem.Closed.Coerce args (LayoutHeader' msg) ⇒
  args →
  Builder m msg widget i o →
  Builder m msg widget i o
closeSection args (Builder (B.BuilderD bd)) =
  builder do
    { default: d, dualD } ← bd
    pure
      { default:
        d
          >>= \{ layout, payload } → do
              let
                layout' = close' layout
              pure { layout: layout', payload }
      , dualD: un Polyform.Dual (Reporter.Dual.lmapM (close >>> pure) (Polyform.Dual dualD))
      }
  where
  header = NoProblem.Closed.coerce args ∷ LayoutHeader' msg

  -- | Make this polymorphic
  close' s = do
    let
      title = NoProblem.toMaybe header.title
    Layout.closeSection { id: NoProblem.toMaybe header.id, title } s

  close s = do
    let
      title = NoProblem.toMaybe header.title
    Layout.closeSection { id: NoProblem.toMaybe header.id, title } s

sectionDual ∷
  ∀ i m msg o widget.
  Monad m ⇒
  FieldDual m msg i o →
  Builder m msg widget i o
sectionDual d = builder $ pure { default: pure mempty, dualD }
  where
  Polyform.Dual dualD = Reporter.Dual.liftValidatorDualWith (Tuple.snd >>> Layout.sectionErrors) (const mempty) d

-- | TODO: We should probably differenciate between monads here....
default ∷
  ∀ m msg n o widget.
  Bi m n msg widget o →
  n (B.Default (Layout msg widget))
default (Bi form) = Form.default form

-- | Make this consistnent with the above
-- | probably we want to use just `Tuple` everywhere.
serialize ∷
  ∀ m msg n o widgets.
  Applicative m ⇒
  Bi m n msg widgets o →
  o →
  n (Tuple UrlDecoded (Layout msg widgets))
serialize (Bi form) = Form.serialize form

validate ∷
  ∀ m msg o widgets.
  Monad m ⇒
  Bi m m msg widgets o →
  UrlDecoded →
  m (Tuple (Maybe o) (Layout msg widgets))
validate (Bi form) = Form.validate form

build ∷
  ∀ m msg o widgets.
  Builder m msg widgets UrlDecoded o →
  Bi m m msg widgets o
build (Builder (B.BuilderD b)) = Bi $ Form.Form $ (\r@{ dualD } → { default: r.default, dual: Polyform.Dual r.dualD }) <<< BuilderM.eval $ b

dual ∷
  ∀ m msg o widgets.
  Bi m m msg widgets o →
  Dual m msg widgets UrlDecoded o
dual (Bi (Form.Form form)) = form.dual

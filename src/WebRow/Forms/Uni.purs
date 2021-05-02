module WebRow.Forms.Uni
  ( build
  , default
  , defaultM
  , Builder
  , FieldValidator
  , PasswordInputInitials
  , TextInputInitials
  , TextInputInitialsBase
  , Uni
  , closeSection
  , fieldBuilder
  , emailInputBuilder
  , optEmailInputBuilder
  , passwordInputBuilder
  , optPasswordInputBuilder
  , sectionValidator
  , textInputBuilder
  , validate
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as NoProblem.Closed
import Polyform (Reporter, Validator) as Polyform
import Polyform.Batteries (Msg)
import Polyform.Batteries.UrlEncoded.Validators (MissingValue, optValidator)
import Polyform.Batteries.UrlEncoded.Validators (value) as Batteries
import Polyform.Reporter (liftFn, liftValidatorWith, lmapReporter) as Reporter
import Polyform.Validator (liftFn) as Validator
import Type.Row (type (+))
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Layout (LayoutBase(..), closeSection, sectionErrors) as Layout
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Uni.Builder (Builder(..)) as B
import WebRow.Forms.Uni.Form (Form(..), default, validate) as Form
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Validators (email) as Validators
import WebRow.Forms.Widget (Constructor, Payload, Names, names, payload) as Widget
import WebRow.Forms.Widgets (TextInput)
import WebRow.Forms.Widgets (textInput) as Widgets
import WebRow.Mailer (Email)

type FieldValidator m msg i o
  = Polyform.Validator m (Array msg) i o

newtype Builder m msg widget i o
  = Builder
  (B.Builder m (Layout msg widget) i o)

newtype Uni m msg widget o
  = Uni (Form.Form m (Layout msg widget) o)

-- | Not able to derive one - probably monkind issue
derive newtype instance functorBuilder ∷ Applicative m ⇒ Functor (Builder m msg widget i)

derive newtype instance applyBuilder ∷ Monad m ⇒ Apply (Builder m msg widget i)

derive newtype instance applicativeBuilder ∷ Monad m ⇒ Applicative (Builder m msg widget i)

derive newtype instance semigroupoidBuilder ∷ Monad m ⇒ Semigroupoid (Builder m msg widget)

derive newtype instance categoryBuilder ∷ Monad m ⇒ Category (Builder m msg widget)

-- | TODO: Custom names are currently NOT validated.
-- | We are not able (and don't want) to keep this
-- | info on the type type level (we would lose `Monoid`
-- | instance with such a `Row`).
-- | We probably want keep track of used names and provide
-- | validation on value level during "`safeBuild`".
fieldBuilder ∷
  ∀ inputs m msg o widget.
  Traversable inputs ⇒
  Monad m ⇒
  Monoid (inputs Unit) ⇒
  { constructor ∷ Widget.Constructor msg inputs widget o
  , defaults ∷ Widget.Payload inputs
  , name ∷ Maybe (Widget.Names inputs)
  , widgetId ∷ Maybe String
  , validator ∷ FieldValidator m msg (Widget.Payload inputs) o
  } →
  Builder m msg widget UrlDecoded o
fieldBuilder { constructor, defaults, name, validator: fieldValidator, widgetId } =
  Builder $ B.Builder
    $ do
        ns ← case name of
          Nothing → Widget.names
          Just n → pure n
        let
          constructor' = step <<< constructor
            where
              step widget = Layout.Widget { id: widgetId, widget }

          fromSuccess ∷ Tuple (Widget.Payload inputs) o → Layout msg widget
          fromSuccess (Tuple payload o) = constructor' { payload, names: ns, result: Just (Right o) }

          fromFailure ∷ Tuple (Widget.Payload inputs) (Array msg) → Layout msg widget
          fromFailure (Tuple payload e) = constructor' { payload, names: ns, result: Just (Left e) }

          widgetValidator ∷ Polyform.Reporter m (Layout msg widget) (Widget.Payload inputs) o
          widgetValidator = Reporter.liftValidatorWith fromFailure fromSuccess fieldValidator

          reporter =
            widgetValidator
              <<< Reporter.liftFn (Widget.payload ns)
        pure
          $ { reporter
            , default: constructor' { payload: defaults, names: ns, result: Nothing }
            }

foreign import kind OptFlag

foreign import data Optional ∷ OptFlag

foreign import data Required ∷ OptFlag

-- | We reuse this row in Forms.Bi
type TextInputInitialsBase msg (r ∷ #Type)
  = ( default ∷ Opt String
    , helpText ∷ Opt msg
    , label ∷ Opt msg
    , name ∷ Opt String
    , type_ ∷ Opt String
    , placeholder ∷ Opt msg
    , widgetId ∷ Opt String
    | r
    )

type TextInputInitials m msg o
  = {
    | TextInputInitialsBase msg
      + ( validator ∷ FieldValidator m msg (Maybe Payload.Value) o )
    }

textInputBuilder ∷
  ∀ args m msg o widget.
  Monad m ⇒
  NoProblem.Closed.Coerce args (TextInputInitials m msg o) ⇒
  args →
  Builder m msg (TextInput () + widget) UrlDecoded o
textInputBuilder args =
  fieldBuilder
    { constructor
    , defaults: Identity (Just [ default ! "" ])
    , name: Identity <$> NoProblem.toMaybe i.name
    , validator: validator'
    , widgetId: NoProblem.toMaybe i.widgetId
    }
  where
  i@{ default, validator } = NoProblem.Closed.coerce args ∷ TextInputInitials m msg o

  validator' = validator <<< Validator.liftFn (un Identity)

  constructor { payload: Identity payload, names: Identity name, result } = do
    let
      helpText = NoProblem.toMaybe i.helpText
      label = NoProblem.toMaybe i.label
      placeholder = NoProblem.toMaybe i.placeholder
    Widgets.textInput
      { type_: i.type_ ! "text"
      , helpText
      , label
      , payload
      , placeholder
      , name
      , result: either Just (const Nothing) <$> result
      }

type PasswordInputInitials m msg
  = { helpText ∷ Opt msg
    , label ∷ Opt msg
    , name ∷ Opt String
    , placeholder ∷ Opt msg
    , policy ∷ Opt (FieldValidator m msg String String)
    }

passwordInputBuilder ∷
  ∀ args m msg r.
  Monad m ⇒
  NoProblem.Closed.Coerce args (PasswordInputInitials m (Msg (MissingValue + msg))) ⇒
  args →
  Builder
    m
    (Msg (MissingValue + msg))
    (TextInput () + r)
    UrlDecoded
    String
passwordInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , name
    , type_: "password"
    , validator: Batteries.value >>> (i.policy ! identity)
    }
  where
  i@{ helpText, label, name, placeholder } = NoProblem.Closed.coerce args ∷ PasswordInputInitials m (Msg (MissingValue + msg))

optPasswordInputBuilder ∷
  ∀ args m msg r.
  Monad m ⇒
  NoProblem.Closed.Coerce args (PasswordInputInitials m (Msg msg)) ⇒
  args →
  Builder
    m
    (Msg msg)
    (TextInput () + r)
    UrlDecoded
    (Maybe String)
optPasswordInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , name
    , type_: "password"
    , validator: optValidator (i.policy ! identity)
    }
  where
  i@{ helpText, label, name, placeholder } = NoProblem.Closed.coerce args ∷ PasswordInputInitials m (Msg msg)

type EmailMessages r
  = InvalidEmailFormat + r

type EmailInputInitials m msg
  = { label ∷ Opt msg
    , name ∷ Opt String
    , placeholder ∷ Opt msg
    , helpText ∷ Opt msg
    , policy ∷ Opt (FieldValidator m msg Email Email)
    }

emailInputBuilder ∷
  ∀ args m msg r.
  Monad m ⇒
  NoProblem.Closed.Coerce
    args
    (EmailInputInitials m (Msg (MissingValue + EmailMessages + msg))) ⇒
  args →
  Builder
    m
    (Msg (EmailMessages + MissingValue + msg))
    (TextInput () + r)
    UrlDecoded
    Email
emailInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , name
    , type_: "email"
    , validator: Batteries.value >>> Validators.email >>> (i.policy ! identity)
    }
  where
  i@{ helpText, label, name, placeholder } = NoProblem.Closed.coerce args ∷ EmailInputInitials m (Msg (EmailMessages + MissingValue + msg))

optEmailInputBuilder ∷
  ∀ args m msg r.
  Monad m ⇒
  NoProblem.Closed.Coerce
    args
    (EmailInputInitials m (Msg (EmailMessages + msg))) ⇒
  args →
  Builder
    m
    (Msg (EmailMessages + msg))
    (TextInput () + r)
    UrlDecoded
    (Maybe Email)
optEmailInputBuilder args =
  textInputBuilder
    { placeholder
    , helpText
    , label
    , name
    , type_: "email"
    , validator: optValidator (Validators.email >>> (i.policy ! identity))
    }
  where
  i@{ helpText, label, name, placeholder } = NoProblem.Closed.coerce args ∷ EmailInputInitials m (Msg (EmailMessages + msg))

sectionValidator ∷
  ∀ i m msg o widgets.
  Monad m ⇒
  Polyform.Validator m (Array msg) i o →
  Builder m msg widgets i o
sectionValidator validator = Builder $ B.Builder $ pure { default: mempty, reporter: reporter }
  where
  reporter = Reporter.liftValidatorWith (Tuple.snd >>> Layout.sectionErrors) (const mempty) validator

type LayoutHeader' msg =
  { id ∷ Opt String, title ∷ Opt msg }

closeSection ∷
  ∀ args i m msg o widgets.
  Monad m ⇒
  NoProblem.Closed.Coerce args (LayoutHeader' msg) ⇒
  args →
  Builder m msg widgets i o →
  Builder m msg widgets i o
closeSection args (Builder (B.Builder b)) =
  Builder
    $ B.Builder do
        { default: d, reporter } ← b
        pure { default: close d, reporter: Reporter.lmapReporter close reporter }
  where
  header = NoProblem.Closed.coerce args ∷ (LayoutHeader' msg)
  close s = do
    let
      title = NoProblem.toMaybe header.title
    Layout.closeSection { id: NoProblem.toMaybe header.id, title } s

build ∷
  ∀ m msg o widget.
  Builder m msg widget UrlDecoded o →
  Uni m msg widget o
build (Builder (B.Builder b)) = Uni $ Form.Form $ BuilderM.eval b

default ∷
  ∀ m msg widgets o.
  Uni m msg widgets o →
  Layout msg widgets
default (Uni form) = Form.default form

-- | Trivial helper to extract default without annotation in a given monad.
defaultM ∷
  ∀ m msg widgets o.
  Applicative m ⇒
  Uni m msg widgets o →
  m (Layout msg widgets)
defaultM (Uni form) = pure $ Form.default form

validate ∷
  ∀ m msg o widgets.
  Functor m ⇒
  Uni m msg widgets o →
  UrlDecoded →
  m (Tuple (Maybe o) (Layout msg widgets))
validate (Uni form) i = Form.validate form i


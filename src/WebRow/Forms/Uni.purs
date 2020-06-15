module WebRow.Forms.Uni
  ( build
  , default
  , Builder
  , FieldValidator
  , Layout
  , MessageM
  , PasswordInputInitials
  , TextInputInitials
  , TextInputInitialsBase
  , Uni
  , closeSection
  , fieldBuilder
  , emailInputBuilder
  , passwordInputBuilder
  , sectionValidator
  , textInputBuilder
  , validate
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Data.Undefined.NoProblem.Mono (class Coerce, coerce) as NoProblem.Mono
import Data.Undefined.NoProblem.Poly (class Coerce, coerce) as NoProblem.Poly
import Data.Variant (Variant)
import Polyform (Reporter, Validator) as Polyform
import Polyform.Batteries (Errors)
import Polyform.Batteries.UrlEncoded.Validators (SingleValueExpected)
import Polyform.Batteries.UrlEncoded.Validators (singleValue) as Batteries
import Polyform.Reporter (liftFn, liftValidatorWith, liftValidatorWithM, lmapM) as Reporter
import Polyform.Validator (liftFn, lmapM) as Validator
import Run (Run)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import WebRow.Forms.BuilderM (eval) as BuilderM
import WebRow.Forms.Layout (Layout, LayoutBase(..), closeSection, sectionErrors) as Layout
import WebRow.Forms.Payload (UrlDecoded)
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Uni.Builder (Builder(..)) as B
import WebRow.Forms.Uni.Form (Form(..), default, validate) as Form
import WebRow.Forms.Validators (InvalidEmailFormat)
import WebRow.Forms.Validators (email) as Validators
import WebRow.Forms.Widget (Constructor, Payload, names, payload) as Widget
import WebRow.Forms.Widgets (TextInput)
import WebRow.Forms.Widgets (textInput) as Widgets
import WebRow.Mailer (Email)
import WebRow.Message (MESSAGE, message)

type Layout widgets = Layout.Layout String widgets

type FieldValidator eff info i o = Polyform.Validator (MessageM info eff) (Errors info) i o

type MessageM info eff = Run (message ∷ MESSAGE info | eff)

newtype Builder eff info widgets i o = Builder
  (B.Builder (MessageM info eff) (Layout widgets) i o)

newtype Uni eff info widgets o = Uni (Form.Form (MessageM info eff) (Layout widgets) o)

-- | Not able to derive one - probably monkind issue
derive newtype instance functorBuilder ∷ Functor (Builder eff info widgets i)
derive newtype instance applyBuilder ∷ Apply (Builder eff info widgets i)
derive newtype instance applicativeBuilder ∷ Applicative (Builder eff info widgets i)
derive newtype instance semigroupoidBuilder ∷ Semigroupoid (Builder eff info widgets)
derive newtype instance categoryBuilder ∷ Category (Builder eff info widgets)

fieldBuilder
  ∷ ∀ eff info inputs o widgets
  . Traversable inputs
  ⇒ Monoid (inputs Unit)
  ⇒ { constructor ∷ Widget.Constructor (MessageM info eff) inputs widgets o
    , defaults ∷ Widget.Payload inputs
    , validator ∷ FieldValidator eff info (Widget.Payload inputs) o
    }
  → Builder eff info widgets UrlDecoded o
fieldBuilder { constructor, defaults, validator: msgValidator } = Builder $ B.Builder $ do
  ns ← Widget.names
  let
    constructor' = map Layout.Widget <<< constructor

    fromSuccess ∷ Tuple (Widget.Payload inputs) o → MessageM info eff (Layout widgets)
    fromSuccess (Tuple payload o) =
      constructor' { payload, names: ns, result: Just (Right o) }

    fromFailure ∷ Tuple (Widget.Payload inputs) (Errors info) → MessageM info eff (Layout widgets)
    fromFailure (Tuple payload e) = do
      e' ← traverse message e
      constructor' { payload, names: ns, result: Just (Left e') }

    widgetValidator ∷ Polyform.Reporter (MessageM info eff) (Layout widgets) (Widget.Payload inputs) o
    widgetValidator = Reporter.liftValidatorWithM fromFailure fromSuccess msgValidator

    validator
      = widgetValidator
      <<< Reporter.liftFn ( Widget.payload ns )

  pure $ unsafeCoerce 8
    { validator
    , default: constructor' { payload: defaults, names: ns, result: Nothing }
    }

-- | We reuse this row in Forms.Bi
type TextInputInitialsBase (r ∷ # Type) =
  ( default ∷ Opt String
  , label ∷ Opt String
  , type_ ∷ Opt String
  , placeholder ∷ Opt String
  , helpText ∷ Opt String
  | r
  )

type TextInputInitials info eff o =
  { | TextInputInitialsBase
    + ( validator ∷ Polyform.Validator (MessageM info eff) (Errors info) (Maybe Payload.Value) o )
  }

textInputBuilder
  ∷ ∀ args eff info o widgets
  . NoProblem.Poly.Coerce args (TextInputInitials info eff o) (TextInputInitials info eff o)
  ⇒ args
  → Builder eff info (TextInput + widgets) UrlDecoded o
textInputBuilder args = fieldBuilder { constructor, defaults: Identity (Just [ default ! "" ]), validator: validator' }
  where
    i@{ default, validator } = NoProblem.Poly.coerce (Proxy ∷ Proxy (TextInputInitials info eff o)) args
    validator' = validator <<< Validator.liftFn (un Identity)

    constructor { payload: Identity payload, names: Identity name, result } = pure $ Widgets.textInput
      { type_: i.type_ ! "text"
      , helpText: i.helpText # NoProblem.toMaybe
      , label: i.label # NoProblem.toMaybe
      , payload
      , placeholder: i.placeholder # NoProblem.toMaybe
      , name
      , result: map (mkExists <<< Identity) <$> result
      }

type PasswordInputInitials =
  { label ∷ Opt String
  , placeholder ∷ Opt String
  , helpText ∷ Opt String
  }

passwordInputBuilder
  ∷ ∀ args eff info r
  . NoProblem.Mono.Coerce args PasswordInputInitials
  ⇒ args
  → Builder
    eff
    (SingleValueExpected + info)
    (TextInput + r)
    UrlDecoded
    String
passwordInputBuilder args = textInputBuilder
  { placeholder
  , helpText
  , label
  , validator: Batteries.singleValue
  }
  where
    i@{ helpText, label, placeholder } = NoProblem.Mono.coerce args ∷ PasswordInputInitials

type EmailMessages r = InvalidEmailFormat + SingleValueExpected + r

type EmailInputInitials eff info =
  { label ∷ Opt String
  , placeholder ∷ Opt String
  , helpText ∷ Opt String
  , policy ∷ Opt (Polyform.Validator (MessageM (EmailMessages + info) eff) (Errors (EmailMessages + info)) Email Email)
  }

emailInputBuilder
  ∷ ∀ args eff info r
  . NoProblem.Poly.Coerce
      args
      (EmailInputInitials eff info)
      (EmailInputInitials eff info)
  ⇒ args
  → Builder
      eff
      (EmailMessages info)
      (TextInput + r)
      UrlDecoded
      Email
emailInputBuilder args = textInputBuilder
  { placeholder
  , helpText
  , label
  , validator: Batteries.singleValue >>> Validators.email >>> (i.policy ! identity)
  }
  where
    i@{ helpText, label, placeholder } = NoProblem.Poly.coerce (Proxy ∷ Proxy (EmailInputInitials eff info)) args

sectionValidator
  ∷ ∀ eff i info o widgets
  . Polyform.Validator (MessageM info eff) (Errors info) i o
  → Builder eff info widgets i o
sectionValidator validator = Builder $ B.Builder $ pure { default: pure mempty, reporter: reporter }
  where
    validator' = Validator.lmapM (traverse message) validator
    reporter = Reporter.liftValidatorWith (Tuple.snd >>> Layout.sectionErrors) (const mempty) validator'

closeSection ∷ ∀ eff i info o widgets. Variant info → Builder eff info widgets i o → Builder eff info widgets i o
closeSection title (Builder (B.Builder b)) = Builder $ B.Builder do
  { default: d, reporter } ← b
  pure { default: d >>= close, reporter: Reporter.lmapM close reporter }
  where
    close s = do
      t ← message title
      pure $ Layout.closeSection t s

build
  ∷ ∀ eff info o widgets
  . Builder eff info widgets UrlDecoded o
  → Uni eff info widgets o
build (Builder (B.Builder b)) = Uni $ Form.Form $ BuilderM.eval b

default :: forall eff info widgets o.
   Uni eff info widgets o
   -> MessageM info eff (Layout widgets)
default (Uni form) = Form.default form

validate
  ∷ ∀ eff info o widgets
  . Uni eff info widgets o
  → UrlDecoded
  → MessageM info eff (Tuple (Layout widgets) (Maybe o))
validate (Uni form) i = Form.validate form i

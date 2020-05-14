module WebRow.Forms.Plain where

import Prelude

import Control.Monad.State (evalState)
import Control.Monad.State (modify) as State
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Variant (Variant, inj)
import Polyform.Reporter (R(..), hoistFn, hoistValidatorWith, lmapReporter, runReporter) as Reporter
import Polyform.Reporter (Reporter) as Polyform.Reporter
import Polyform.Validator (Validator) as Polyform.Validator
import Polyform.Validator (hoistFnEither) as Validator
import Polyform.Validators.UrlEncoded (string)
import Record (insert) as Record
import Type.Prelude (SProxy(..))
import Type.Prelude (reflectSymbol) as Symbol
import WebRow.Forms.Layout (Layout(..), closeSection, sectionError) as Layout
import WebRow.Forms.Payload (Value) as Payload
import WebRow.Forms.Payload (UrlDecoded)

type Message msg = Variant ( | msg)

type Layout field msg = Layout.Layout field msg

newtype Sequence a = Sequence (StateT Int Identity a)
derive newtype instance functorSequence ∷ Functor Sequence
derive newtype instance applySequence ∷ Apply Sequence
derive newtype instance applicativeSequence ∷ Applicative Sequence
derive newtype instance bindSequence ∷ Bind Sequence
derive newtype instance monadSequence ∷ Monad Sequence

next ∷ Sequence Int
next = Sequence $ State.modify (_ + 1)

evalSequence ∷ ∀ a. Sequence a → a
evalSequence (Sequence a) = evalState a 0

name ∷ Sequence String
name = do
  id ← next
  pure $ "field-" <> show id

-- | `default` should probably differ from result layout because
-- | it should be able to represent "unvalidated" input.
-- | We are going going to wrap result in a Maybe everywhere
-- | to accomodate this inconsistency ;-)
newtype Form m field msg i o = Form
  ( Sequence
      { default ∷ Layout msg field
      , reporter ∷ Polyform.Reporter.Reporter m (Layout msg field) i o
      }
  )

derive instance newtypeForm ∷ Newtype (Form m l e i o) _
derive instance functorForm ∷ (Applicative m) ⇒ Functor (Form m field msg i)

instance applicativeForm ∷ (Applicative m) ⇒ Apply (Form m field msg i) where
  apply (Form sw1) (Form sw2) = Form $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { default: w1.default <> w2.default
      , reporter: apply w1.reporter w2.reporter
      }

instance semigroupoidForm ∷ (Monad m) ⇒ Semigroupoid (Form m field msg) where
  compose (Form sw1) (Form sw2) = Form $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { default: w1.default <> w2.default
      , reporter: compose w1.reporter w2.reporter
      }

instance categoryForm ∷ (Monad m) ⇒ Category (Form m field msg) where
  identity = Form $ pure
    { default: mempty
    , reporter: identity
    }

type InputFieldRecord msg result =
  { name ∷ String, input ∷ Maybe Payload.Value, result ∷ Maybe (Either (Array msg) result), type_ ∷ String }

type FieldRow msg extra =
  ( "textInput" ∷ InputFieldRecord msg String
  -- , "numberInput" ∷ { name ∷ String, input ∷ Maybe (Array String), result ∷ Maybe (Either msg Number) }
  | extra
  )

-- | TODO: For convenience we can provide a version which is `o` agnostic
-- | in case of the constructor. In such a case we can (Maybe msg) as an result
-- | representation.
field
  ∷ ∀ field m msg o
  . Monad m
  ⇒ ({ input ∷ Maybe Payload.Value, name ∷ String, result ∷ Maybe (Either (Array msg) o) } → field)
  → { defaults ∷ { input ∷ Maybe Payload.Value, result ∷ Maybe (Either (Array msg) o)}
    , validator ∷ Polyform.Validator.Validator m (Array msg) (Maybe Payload.Value) o
    }
  → Form m field msg UrlDecoded o
field build { defaults, validator } = Form $ do
  n ← name
  let
    build' = Layout.Field <<< build <<< Record.insert (SProxy ∷ SProxy "name") n

    fromFailure (Tuple input err) = build' { input, result: Just (Left err) }
    fromSuccess (Tuple input result) = build' { input, result: Just (Right result) }

    reporter
      = Reporter.hoistValidatorWith fromFailure fromSuccess (validator)
      <<< Reporter.hoistFn (Map.lookup n)

    default = build' defaults
  pure { default, reporter }

_textInput = SProxy ∷ SProxy "textInput"
_type = SProxy ∷ SProxy "type_"

textInput default type_ validator = field build { defaults: { input: Just [ default ], result: Nothing }, validator }
  where
    build r = inj _textInput (Record.insert _type type_ r)

input default label validator = field build { defaults: { input: Just [ default ], result: Nothing }, validator }
  where
    build r = inj label (Record.insert _type (Symbol.reflectSymbol label) r)

nonEmptyString = string >>> Validator.hoistFnEither \p → case p of
  "" → Left ["Value is required"]
  otherwise → Right p

passwordField = textInput "" "password" nonEmptyString

sectionValidator ∷ ∀ field i m msg o. Functor m ⇒ Polyform.Validator.Validator m msg i o → Form m field msg i o
sectionValidator validator = Form $ pure { default: mempty, reporter: reporter }
  where
    reporter = Reporter.hoistValidatorWith (Tuple.snd >>> Layout.sectionError) (const mempty) validator

closeSection ∷ ∀ field i m msg o. Monad m ⇒ msg → Form m field msg i o → Form m field msg i o
closeSection title (Form form) = Form $ do
  { default, reporter } ← form
  pure { default: close default, reporter: Reporter.lmapReporter close reporter }
  where
    close = Layout.closeSection title

run
  ∷ ∀ field m msg o
  . Monad m
  ⇒ Form m field msg UrlDecoded o
  → UrlDecoded
  → m (Tuple (Layout msg field) (Maybe o))
run (Form form) input = case evalSequence form of
  { reporter } → Reporter.runReporter reporter input >>= case _ of
    Reporter.Success r o → pure $ Tuple r (Just o)
    Reporter.Failure r → pure $ Tuple r Nothing

default (Form form) = _.default <<< evalSequence $ form

defaultM ∷ ∀ m t185 t186 t187 t188 t189 t193. Applicative m ⇒ Form m t188 t187 t186 t185 → m (Layout t187 t188)
defaultM (Form form) = pure <<<_.default <<<  evalSequence $ form

module WebRow.Forms.Dual where

import Prelude

import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Identity (Identity(..))
import Data.Map (lookup, singleton) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple (fst) as Tuple
import Data.Variant (inj) as Variant
import Polyform.Dual (Dual(..), dual, parser, serializer) as Dual
import Polyform.Duals.Reporter (DualD) as Polyform.Duals.Reporter
import Polyform.Duals.Reporter (hoistSerializerWith, hoistValidatorWith, runReporter) as Duals.Reporter
import Polyform.Duals.Validator (Dual) as Polyform.Duals.Validator
import Polyform.Reporter (R(..), hoistFn) as Reporter
import Record (insert) as Record
import WebRow.Forms.Fields (Initials, ValidationCtx) as Fields
import WebRow.Forms.Fields (_name, _textInput)
import WebRow.Forms.Layout (Layout(..)) as Layout
import WebRow.Forms.Layout (Layout)
import WebRow.Forms.Payload (Value, UrlDecoded) as Payload
import WebRow.Forms.Sequence (Sequence)
import WebRow.Forms.Sequence (eval, id) as Sequence

type SerializerCtx msg field = Tuple (Layout msg field)

newtype FormD m field msg i o' o = FormD
  ( Sequence
      { dualD ∷ Polyform.Duals.Reporter.DualD m (SerializerCtx msg field) (Layout msg field) i o' o
      , default ∷ Layout msg field
      }
  )

derive instance functorFormD ∷ Functor m ⇒ Functor (FormD m field msg i o')

instance applyFormD ∷ (Semigroup i, Applicative m) ⇒ Apply (FormD m field msg i o') where
  apply (FormD sw1) (FormD sw2) = FormD $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { dualD: apply w1.dualD w2.dualD
      , default: w1.default <> w2.default
      }
instance applicativeFormD ∷ (Monoid i, Applicative m) ⇒ Applicative (FormD m field msg i o') where
  pure a = FormD $ pure
    { dualD: pure a
    , default: mempty
    }

instance profunctorFormD ∷ (Functor m) ⇒ Profunctor (FormD m field msg i) where
  dimap l r (FormD w) = FormD do
    { dualD, default } ← w
    pure { dualD: dimap l r dualD, default }

newtype Form m field msg i o = Form (FormD m field msg i o o)
derive instance newtypeForm ∷ Newtype (Form m field msg i o) _

instance semigroupoidForm ∷ (Monad m) ⇒ Semigroupoid (Form m field msg) where
  compose (Form (FormD sw1)) (Form (FormD sw2)) = Form $ FormD $ do
    w1 ← sw1
    w2 ← sw2
    pure
      { dualD: un Dual.Dual (compose (Dual.Dual w1.dualD) (Dual.Dual w2.dualD))
      , default: w1.default <> w2.default
      }

instance categoryForm ∷ (Monad m) ⇒ Category (Form m field msg) where
  identity = Form $ FormD $ pure { dualD: un Dual.Dual identity, default: mempty }

infixl 5 diverge as ~

diverge
  ∷ ∀ field i m msg o o'
  . Functor m
  ⇒ (o' → o)
  → Form m field msg i o
  → FormD m field msg i o' o
diverge f = lcmap f <<< un Form

field
  ∷ ∀ field m msg o
  . Monad m
  ⇒ (Fields.Initials msg o → field)
  → { defaults ∷ Fields.ValidationCtx () msg o
    , dual ∷ Polyform.Duals.Validator.Dual m Identity (Array msg) (Maybe Payload.Value) o
    }
  → Form m field msg Payload.UrlDecoded o
field constructor { defaults, dual } = Form $ FormD $ do
  n ← Sequence.id
  let
    fromInitials = Layout.Field <<< constructor <<< Record.insert _name n
    fromFailure (Tuple input err) = fromInitials { input, result: Just (Left err) }
    fromSuccess (Tuple input result) = fromInitials { input, result: Just (Right result) }

    hoistSerializer (Identity s@(Tuple i o)) = Tuple (fromSuccess s) i

    -- | This lifts field `Duals.Validator.Dual` which reports only errors
    -- | into the `Form` builder as well.
    fieldDual
      = Duals.Reporter.hoistSerializerWith hoistSerializer
      <<< Duals.Reporter.hoistValidatorWith fromFailure fromSuccess
      $ dual

    -- | This lifts our duals to work on `Payload.UrlDecoded` and not on `Payload.Value`
    dualD
      = un Dual.Dual
      $ fieldDual
      <<< Dual.dual (Reporter.hoistFn (Map.lookup n)) (maybe mempty (Map.singleton n >>> pure))

    default = fromInitials defaults

  pure { default, dualD }


textInput default type_ dual = field constructor { defaults: { input: Just [ default ], result: Nothing }, dual }
  where
    constructor { input, name, result } = Variant.inj
      _textInput
      { type_: type_
      , input
      , name
      , result: map (mkExists <<< Identity) <$> result
      }

run
  ∷ ∀ field m msg o
  . Monad m
  ⇒ Form m field msg Payload.UrlDecoded o
  → Payload.UrlDecoded
  → m (Tuple (Layout msg field) (Maybe o))
run (Form (FormD form)) input = case Sequence.eval form of
  { dualD } → Duals.Reporter.runReporter (Dual.Dual dualD) input >>= case _ of
    Reporter.Success r o → pure $ Tuple r (Just o)
    Reporter.Failure r → pure $ Tuple r Nothing

build :: forall field o m msg. Form m field msg Payload.UrlDecoded o -> o -> Layout msg field
build (Form (FormD form)) value
  = Tuple.fst
  <<< flip Dual.serializer value
  <<< Dual.Dual
  <<< _.dualD
  <<< Sequence.eval
  $ form


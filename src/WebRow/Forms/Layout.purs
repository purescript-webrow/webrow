module WebRow.Forms.Layout where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import WebRow.Forms.Widget (Widget)

-- | Now when we have `VariantF` as a base widget representation and example
-- | we can slowly move to this cleaner representation I think:
-- |
-- | ```purescript
-- | data Layout (widget :: Type -> Type) msg
-- |   = Section (Array msg) (Array (Layout widget msg))
-- |   | Widget (widget msg)
-- |
-- | derive instance functorLayout :: Functor widget => Functor (Layout widget)
-- |
-- | hoistWidget ::
-- |   forall msg widget widget'.
-- |   (widget ~> widget') -> Layout widget msg -> Layout widget' msg
-- | hoistWidget f (Widget w) = Widget (f w)
-- | hoistWidget f (Section msgs layouts) = Section msgs (map (hoistWidget f) layouts)
-- | ```

-- | Because widegts has a row kind (# Type) I'm not able to
-- | use this type directly. I would not be able
-- | to provide instances for such a `Layout` type...
type Layout message widget
  = LayoutBase message (Widget widget message)

mapMessage ∷ ∀ message message' widget. (message → message') → Layout message widget → Layout message' widget
mapMessage f l = bimap f (map f) l

-- | `Layout` is a proposition for the form UI representation.
-- | Provided DSLs in `Forms.Builders` depend on this structure
-- | of a layout but in general you can switch layout representation
-- | if you want to. Form types (`Forms.Plain` and `Form.Dual`) are polymorphic
-- | over it. They just require a `Monoid` for a proper composition.
-- |
-- | This layout allows you to build tree like structure of sections
-- | which can reference validation information related to a given
-- | section.
-- | Section can be "closed" by providing a title. "Closing" drives the
-- | behaviour of monoidal `append` and allows you to build a tree.
-- |
-- | These references are filled out when validation process is finished.
-- | Please check `Forms.Plain.run` or `Forms.Dual.run`.
-- |
type Header message
  = { id ∷ Maybe String, title ∷ Maybe message }

type Section message widget
  = { closed ∷ Maybe (Header message)
    , errors ∷ Array message
    , layout ∷ List (LayoutBase message widget)
    }

data LayoutBase message widget
  = Section (Section message widget)
  | Widget
    { id ∷ Maybe String
    , widget ∷ widget
    }

derive instance functorLayoutBase ∷ Functor (LayoutBase message)

derive instance eqLayoutBase ∷ (Eq message, Eq widget) ⇒ Eq (LayoutBase message widget)

derive instance genericLayoutBase ∷ Generic (LayoutBase message widgets) _

instance foldableLayoutBase ∷ Foldable (LayoutBase message) where
  foldMap f (Widget { id, widget }) = f widget
  foldMap f (Section { layout }) = foldMap (foldMap f) layout
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableLayoutBase ∷ Traversable (LayoutBase message) where
  sequence (Widget { id, widget }) = Widget <<< { id, widget: _ } <$> widget
  sequence (Section { closed, errors, layout }) = Section <<< { closed, errors, layout: _ } <$> traverse sequence layout
  traverse f = traverseDefault f

instance bifunctorLayoutBase ∷ Bifunctor LayoutBase where
  bimap f g (Section { closed, errors, layout }) =
    Section
      $ { closed: (\r → { id: r.id, title: f <$> r.title }) <$> closed
        , errors: map f errors
        , layout: bimap f g <$> layout
        }
  bimap f g (Widget { id, widget }) = Widget { id, widget: g widget }

instance monoidLayoutBase ∷ Monoid (LayoutBase message widgets) where
  mempty = Section { closed: Nothing, errors: mempty, layout: mempty }


-- <ws> + <ws>
instance semigroupLayoutBase ∷ Semigroup (LayoutBase message widgets) where
  -- | TODO: This not nice and trivial strategy for combining form sections.
  -- | We can do better probably.
  append s1@(Section s1r) s2@(Section s2r) = case s1r.closed, s2r.closed of
    Nothing, Nothing →
      Section
        { closed: Nothing
        , errors: s1r.errors <> s2r.errors
        , layout: s1r.layout <> s2r.layout
        }
    Just _, Just _ →
      Section
        { closed: Nothing
        , errors: mempty
        , layout: s1 : s2 : List.Nil
        }
    Just _, Nothing →
      Section
        { closed: Nothing
        , errors: s2r.errors
        , layout: s1 : s2r.layout
        }
    Nothing, Just _ →
      Section
        { closed: Nothing
        , errors: s1r.errors
        , layout: s1r.layout <> (s2 : List.Nil)
        }
  append s@(Section sr) widget@(Widget _) = case sr.closed of
    Nothing →
      Section
        { closed: Nothing
        , errors: sr.errors
        , layout: sr.layout <> List.singleton widget
        }
    otherwise →
      Section
        { closed: Nothing
        , errors: mempty
        , layout: s : widget : List.Nil
        }
  append widget@(Widget _) s@(Section sr) = case sr.closed of
    Nothing →
      Section
        { closed: Nothing
        , errors: sr.errors
        , layout: widget : sr.layout
        }
    otherwise →
      Section
        { closed: Nothing
        , errors: mempty
        , layout: widget : s : List.Nil
        }
  append widget1@(Widget _) widget2@(Widget _) =
    Section
      { closed: Nothing
      , errors: mempty
      , layout: widget1 : widget2 : List.Nil
      }

closeSection ∷ ∀ message widgets. Header message → LayoutBase message widgets → LayoutBase message widgets
closeSection header widgets@(Widget _) = Section { closed: Just header, layout: List.singleton widgets, errors: mempty }

closeSection header widgets@(Section { layout, errors }) = Section { closed: Just header, layout, errors }

sectionErrors ∷ ∀ message widgets. Array message → LayoutBase message widgets
sectionErrors errors = Section { closed: Nothing, layout: mempty, errors }


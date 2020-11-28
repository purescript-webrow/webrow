module WebRow.Forms.Layout where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton, snoc) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import WebRow.Forms.Widget (Widget)

-- | Because widegts has a row kind (# Type) I'm not able to
-- | use this type directly. I would not be able
-- | to provide instances for such a `Layout` type...
-- | This should be fixed with the next purs release!
type Layout msg widgets
  = LayoutBase msg (Widget widgets)

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

type Section message widgets =
  { closed ∷ Maybe { title ∷ message }
  , errors ∷ Array message
  , layout ∷ List (LayoutBase message widgets)
  }
data LayoutBase message widgets
  = Section (Section message widgets)
  | Widget widgets

derive instance functorLayoutBase ∷ Functor (LayoutBase message)
derive instance genericLayoutBase ∷ Generic (LayoutBase message widgets) _

instance foldableLayoutBase ∷ Foldable (LayoutBase message) where
  foldMap f (Widget widgets) = f widgets
  foldMap f (Section { layout }) = foldMap (foldMap f) layout
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance bifunctorLayoutBase ∷ Bifunctor LayoutBase where
  bimap f g (Section { closed, errors, layout }) =
    Section
      $ { closed: (\r → { title: f r.title }) <$> closed
        , errors: map f errors
        , layout: bimap f g <$> layout
        }
  bimap f g (Widget widgets) = Widget (g widgets)

instance monoidLayoutBase ∷ Monoid (LayoutBase message widgets) where
  mempty = Section { closed: Nothing, errors: mempty, layout: mempty }

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
    Nothing, Just _ →
      Section
        { closed: Nothing
        , errors: s2r.errors
        , layout: s1 : s2r.layout
        }
    Just _, Nothing →
      Section
        { closed: Nothing
        , errors: s1r.errors
        , layout: List.snoc s1r.layout s2
        }
  append s@(Section sr) widgets = case sr.closed of
    Nothing →
      Section
        { closed: Nothing
        , errors: sr.errors
        , layout: widgets : sr.layout
        }
    Just _ →
      Section
        { closed: Nothing
        , errors: mempty
        , layout: s : widgets : List.Nil
        }
  append widgets s@(Section sr) = case sr.closed of
    Nothing →
      Section
        { closed: Nothing
        , errors: sr.errors
        , layout: List.snoc sr.layout widgets
        }
    Just _ →
      Section
        { closed: Nothing
        , errors: mempty
        , layout: widgets : s : List.Nil
        }
  append widgets1 widgets2 =
    Section
      { closed: Nothing
      , errors: mempty
      , layout: widgets1 : widgets2 : List.Nil
      }

closeSection ∷ ∀ message widgets. message → LayoutBase message widgets → LayoutBase message widgets
closeSection title widgets@(Widget _) = Section { closed: Just { title }, layout: List.singleton widgets, errors: mempty }

closeSection title widgets@(Section { layout, errors }) = Section { closed: Just { title }, layout, errors }

sectionErrors ∷ ∀ message widgets. Array message → LayoutBase message widgets
sectionErrors errors = Section { closed: Nothing, layout: mempty, errors }

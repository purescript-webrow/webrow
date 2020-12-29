module WebRow.Forms.Layout where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Variant (class TraversableVFRL, VariantF)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), singleton) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Prim.RowList (class RowToList)
import WebRow.Forms.Widget (Widget)

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
type Header msg
  = { id ∷ Maybe String, title ∷ Maybe msg }

type Section widget msg
  = { closed ∷ Maybe (Header msg)
    , errors ∷ Array msg
    , layout ∷ List (Layout widget msg)
    }

data Layout widget msg
  = Section (Section widget msg)
  | Widget
    { id ∷ Maybe String
    , widget ∷ Widget widget msg
    }

derive instance functorLayoutBase ∷ Functor (Layout widget)

derive instance genericLayoutBase ∷ Generic (Layout widget msg) _

instance foldableLayoutBase ∷ Foldable (VariantF widget) ⇒ Foldable (Layout widget) where
  foldMap f (Widget { id, widget }) = foldMap f widget
  foldMap f (Section { layout }) = foldMap (foldMap f) layout
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableLayout ∷ (RowToList widget rl, TraversableVFRL rl widget) ⇒ Traversable (Layout widget) where
  sequence (Widget { id, widget }) = Widget <<< { id, widget: _ } <$> sequence widget
  sequence (Section r) =
    map Section $ { closed: _, errors: _, layout: _ }
      <$> traverse sequenceHeader r.closed
      <*> sequence r.errors
      <*> traverse sequence r.layout
    where
    sequenceHeader { id, title } = { id, title: _ } <$> sequence title
  traverse f = traverseDefault f

instance monoidLayoutBase ∷ Monoid (Layout widget msg) where
  mempty = Section { closed: Nothing, errors: mempty, layout: mempty }

instance semigroupLayoutBase ∷ Semigroup (Layout widget msg) where
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
        , layout: s1r.layout <> s2 : List.Nil
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

closeSection ∷ ∀ msg widget. Header msg → Layout widget msg → Layout widget msg
closeSection header widget@(Widget _) = Section { closed: Just header, layout: List.singleton widget, errors: mempty }

closeSection header (Section { layout, errors }) = Section { closed: Just header, layout, errors }

sectionErrors ∷ ∀ msg widget. Array msg → Layout widget msg
sectionErrors errors = Section { closed: Nothing, layout: mempty, errors }

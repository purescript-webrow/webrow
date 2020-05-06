module WebRow.Forms.Layout where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.List (List(..), singleton, snoc) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import WebRow.Forms.Validation.Report (Key) as Report

-- | `Layout` it is just a proposition for form layout representation.
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
data Layout field input result
  = Section
    { closed ∷ Maybe { title ∷ String }
    , reports ∷ List result
    , layout ∷ List (Layout field input result)
    }
  | Field { field ∷ field, input ∷ input, result ∷ result }

derive instance functorLayout ∷ Functor (Layout field input)
instance bifunctorLayout ∷ Bifunctor (Layout field) where
  bimap f g (Field { field, input, result }) = Field { field, input: f input, result: g result }
  bimap f g (Section { closed, layout, reports }) = Section
    { closed, layout: map (bimap f g) layout, reports: map g reports }

type EmptyLayout field = Layout Report.Key field

instance semigroupLayout ∷ Semigroup (Layout field input result) where
  append s1@(Section s1r) s2@(Section s2r) = case s1r.closed, s2r.closed of
      Nothing, Nothing → Section
        { closed: Nothing
        , reports: s1r.reports <> s2r.reports
        , layout: s1r.layout <> s2r.layout
        }
      Just _, Just _ → Section
        { closed: Nothing
        , reports: mempty
        , layout: s1 : s2 : List.Nil
        }
      Nothing, Just _ → Section
        { closed: Nothing
        , reports: s2r.reports
        , layout: s1 : s2r.layout
        }
      Just _, Nothing → Section
        { closed: Nothing
        , reports: s1r.reports
        , layout: List.snoc s1r.layout s2
        }

  append s@(Section sr) field = case sr.closed of
    Nothing → Section
      { closed: Nothing
      , reports: sr.reports
      , layout: field : sr.layout
      }
    Just _ → Section
      { closed: Nothing
      , reports: mempty
      , layout: s : field : List.Nil
      }

  append field s@(Section sr) = case sr.closed of
    Nothing → Section
      { closed: Nothing
      , reports: sr.reports
      , layout: List.snoc sr.layout field
      }
    Just _ → Section
      { closed: Nothing
      , reports: mempty
      , layout: field : s : List.Nil
      }

  append field1 field2 = Section
    { closed: Nothing
    , reports: mempty
    , layout: field1 : field2 : List.Nil
    }

closeSection ∷ ∀ field input result. String → Layout field input result → Layout field input result
closeSection title field@(Field _) = Section { closed: Just { title }, layout: List.singleton field, reports: mempty }
closeSection title field@(Section { layout, reports }) = Section { closed: Just { title }, layout, reports }

sectionReporter ∷ ∀ field input result. result → Layout field input result
sectionReporter key = Section { closed: Nothing, layout: mempty, reports: List.singleton key }


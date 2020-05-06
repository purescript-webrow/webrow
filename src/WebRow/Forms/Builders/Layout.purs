module WebRow.Forms.Builders.Layout where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.List (List(..), singleton, snoc) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import WebRow.Forms.Validation.Report (Key) as Report

-- | We want to add probably something like this to the Layout type:
-- | MultiField { reports ∷ Array Result.Key, fields ∷ (Array Fields) }

-- | Section can be "closed" by title definition
data Layout field result
  = Section
    { closed ∷ Maybe { title ∷ String }
    , reports ∷ List result
    , layout ∷ List (Layout field result)
    }
  | Field (Tuple field result)

derive instance functorLayout ∷ Functor (Layout result)
instance bifunctorLayout ∷ Bifunctor Layout where
  bimap f g (Field (Tuple field result)) = Field (Tuple (f field) (g result))
  bimap f g (Section { closed, layout, reports }) = Section
    { closed, layout: map (bimap f g) layout, reports: map g reports }

type EmptyLayout field = Layout Report.Key field

instance semigroupLayout ∷ Semigroup (Layout field result) where
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

closeSection ∷ ∀ field result. String → Layout field result → Layout field result
closeSection title field@(Field _) = Section { closed: Just { title }, layout: List.singleton field, reports: mempty }
closeSection title field@(Section { layout, reports }) = Section { closed: Just { title }, layout, reports }

sectionReporter ∷ ∀ field result. result → Layout field result
sectionReporter key = Section { closed: Nothing, layout: mempty, reports: List.singleton key }


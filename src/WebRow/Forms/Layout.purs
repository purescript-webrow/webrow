module WebRow.Forms.Layout where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.List (List(..), singleton, snoc) as List
import Data.List (List, (:))
import Data.Maybe (Maybe(..))

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
data Layout message field
  = Section
    { closed ∷ Maybe { title ∷ message }
    , errors ∷ List message
    , layout ∷ List (Layout message field)
    }
  | Field field

derive instance functorLayout ∷ Functor (Layout message)
instance bifunctorLayout ∷ Bifunctor Layout where
  bimap f g (Section { closed, errors, layout }) = Section $
    { closed: (\r → { title: f r.title }) <$> closed
    , errors: map f errors
    , layout: bimap f g <$> layout
    }
  bimap f g (Field field) = Field (g field)

instance monoidLayout ∷ Monoid (Layout message field) where
  mempty = Section { closed: Nothing, errors: mempty, layout: mempty }

instance semigroupLayout ∷ Semigroup (Layout message field) where
  append s1@(Section s1r) s2@(Section s2r) = case s1r.closed, s2r.closed of
      Nothing, Nothing → Section
        { closed: Nothing
        , errors: s1r.errors <> s2r.errors
        , layout: s1r.layout <> s2r.layout
        }
      Just _, Just _ → Section
        { closed: Nothing
        , errors: mempty
        , layout: s1 : s2 : List.Nil
        }
      Nothing, Just _ → Section
        { closed: Nothing
        , errors: s2r.errors
        , layout: s1 : s2r.layout
        }
      Just _, Nothing → Section
        { closed: Nothing
        , errors: s1r.errors
        , layout: List.snoc s1r.layout s2
        }

  append s@(Section sr) field = case sr.closed of
    Nothing → Section
      { closed: Nothing
      , errors: sr.errors
      , layout: field : sr.layout
      }
    Just _ → Section
      { closed: Nothing
      , errors: mempty
      , layout: s : field : List.Nil
      }

  append field s@(Section sr) = case sr.closed of
    Nothing → Section
      { closed: Nothing
      , errors: sr.errors
      , layout: List.snoc sr.layout field
      }
    Just _ → Section
      { closed: Nothing
      , errors: mempty
      , layout: field : s : List.Nil
      }

  append field1 field2 = Section
    { closed: Nothing
    , errors: mempty
    , layout: field1 : field2 : List.Nil
    }

closeSection ∷ ∀ message field. message → Layout message field → Layout message field
closeSection title field@(Field _) = Section { closed: Just { title }, layout: List.singleton field, errors: mempty }
closeSection title field@(Section { layout, errors }) = Section { closed: Just { title }, layout, errors }

sectionError ∷ ∀ message field. message → Layout message field
sectionError error = Section { closed: Nothing, layout: mempty, errors: List.singleton error }


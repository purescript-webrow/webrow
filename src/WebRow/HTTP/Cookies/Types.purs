module WebRow.HTTP.Cookies.Types where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

type Name = String
type Value = String
type Values = NonEmptyArray Value

type RequestCookies = Object Values

type SetValue =
  { value ∷ Value
  , attributes ∷ Attributes
  }

data SameSite = Strict | Lax

type Attributes =
  { comment ∷ Maybe String
  , domain ∷ Maybe String
  , expires ∷ Maybe JSDate
  , httpOnly ∷ Boolean
  , maxAge ∷ Maybe Int
  , path ∷ Maybe String
  , sameSite ∷ Maybe SameSite
  , secure ∷ Boolean
  }

defaultAttributes ∷ Attributes
defaultAttributes =
  { comment: Nothing
  , domain: Nothing
  , expires: Nothing
  , httpOnly: false
  , maxAge: Nothing
  , path: Nothing

  , sameSite: Nothing
  , secure: false
  }


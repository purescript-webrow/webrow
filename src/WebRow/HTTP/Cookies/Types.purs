module WebRow.HTTP.Cookies.Types where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.JSDate (JSDate)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object (Object)

type Name
  = String

type Value
  = String

type Values
  = NonEmptyArray Value

type ResponseCookie
  = { value ∷ Value
    , attributes ∷ Attributes
    }

type SetValue
  = ResponseCookie

type RequestCookies
  = Object Values

type ResponseCookies
  = Map Name ResponseCookie

data SameSite
  = Strict
  | Lax

type AttributesRecord
  = { comment ∷ Maybe String
    , domain ∷ Maybe String
    , expires ∷ Maybe JSDate
    , httpOnly ∷ Boolean
    , maxAge ∷ Maybe Int
    , path ∷ Maybe String
    , sameSite ∷ Maybe SameSite
    , secure ∷ Boolean
    }

newtype Attributes
  = Attributes AttributesRecord

derive instance newtypeAttributes ∷ Newtype Attributes _

defaultAttributes ∷ Attributes
defaultAttributes =
  Attributes
    { comment: Nothing
    , domain: Nothing
    , expires: Nothing
    , httpOnly: false
    , maxAge: Nothing
    , path: Just "/"
    , sameSite: Nothing
    , secure: false
    }

attributes ∷ (AttributesRecord → AttributesRecord) → Attributes
attributes f =
  let
    Attributes r = defaultAttributes
  in
    Attributes (f r)

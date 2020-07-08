module WebRow.Testing.HTTP.Cookies where

import Prelude

import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.JSDate (JSDate)
import Data.Map (filter, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple)
import Foreign.Object (fromFoldable) as Object
import WebRow.HTTP.Cookies (Attributes(..), Values) as Cookies
import WebRow.HTTP.Cookies (RequestCookies)
import WebRow.Testing.HTTP.Types (ClientCookies)

-- | We should probably abstract away expiration here
-- | by just taking filtering function for cookies.
dropExpired ∷ Maybe JSDate → ClientCookies → ClientCookies
dropExpired Nothing = identity
dropExpired (Just now) = Map.filter valid
  where
    valid { attributes: Cookies.Attributes { expires }} =
      fromMaybe false ((now < _) <$> expires)

toRequestCookies ∷ ClientCookies -> RequestCookies
toRequestCookies =
  let
    arr ∷ ClientCookies → Array (Tuple String Cookies.Values)
    arr = map (identity *** _.value >>> Array.NonEmpty.singleton) <<< Map.toUnfoldable
  in
    Object.fromFoldable <<< arr

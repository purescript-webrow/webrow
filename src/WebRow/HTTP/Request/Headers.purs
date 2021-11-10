module WebRow.HTTP.Request.Headers
  ( accept
  , accepts
  , headers
  , header
  , MediaPattern(..)
  )
  where

import Prelude

import Data.Array (elem, fromFoldable) as Array
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.String (Pattern(..), split) as String
import HTTPure (Headers, lookup) as HTTPure
import Run (Run)
import Run.Reader (askAt)
import Type.Row (type (+))
import WebRow.HTTP.MediaTypes (parse) as MediaTypes
import WebRow.HTTP.Request.Request (REQUEST, _request)

headers ∷ ∀ eff. Run (REQUEST + eff) HTTPure.Headers
headers = _.headers <$> askAt _request

header ∷ ∀ eff. String → Run (REQUEST + eff) (Maybe String)
header name = flip HTTPure.lookup name <$> headers

data MediaPattern
  = ProperMediaType MediaType
  -- | */*
  | AnyMedia
  -- | image/*
  | AnyImage
  -- | video/*
  | AnyVideo
  | UnkonwnMediaPattern String
derive instance eqMediaPattern ∷ Eq MediaPattern

-- | TODO: Parse also quality factor like q=0.8
-- | https://developer.mozilla.org/en-US/docs/Web/HTTP/Content_negotiation/List_of_default_Accept_values
accept ∷ ∀ eff. Run (REQUEST + eff) (Array { pattern ∷ MediaPattern, q ∷ Maybe String })
accept = do
  parse <$> header "Accept"
  where
    parse mh = do
      h ← Array.fromFoldable mh
      v ← String.split (String.Pattern ",") h
      case String.split (String.Pattern ";") v of
        [ m ] → [{ pattern: mediaPattern m, q: Nothing }]
        [ m, q ] → [{ pattern: mediaPattern m, q: Just q }]
        otherwise → []

    mediaPattern "*/*" = AnyMedia
    mediaPattern "image/*" = AnyImage
    mediaPattern "video/*" = AnyVideo
    mediaPattern p = case MediaTypes.parse p of
      Just mt → ProperMediaType mt
      Nothing → UnkonwnMediaPattern p

-- | TODO: POSSIBLY A BUGGY STUB! We should check for patterns like img/* etc. probably
accepts ∷ ∀ eff. MediaPattern → Run (REQUEST + eff) Boolean
accepts pattern = do
  patterns ← accept
  pure $ pattern `Array.elem` map _.pattern patterns

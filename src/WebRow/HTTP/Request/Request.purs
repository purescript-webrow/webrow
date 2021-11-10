module WebRow.HTTP.Request.Request where

import Prelude

import Data.Array (last) as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split) as String
import HTTPure.Method (Method) as HTTPure
import HTTPure.Request (Request) as HTTPure
import Polyform.Batteries.UrlEncoded (Query)
import Polyform.Batteries.UrlEncoded.Query (parse) as Query
import Run (Run)
import Run.Reader (Reader, askAt, runReaderAt)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type Request = Reader HTTPure.Request

type REQUEST r = ( request ∷ Request | r )

_request = Proxy ∷ Proxy "request"

fullPath ∷ ∀ eff. Run (REQUEST + eff) String
fullPath = _.url <$> askAt _request

body ∷ ∀ eff. Run (REQUEST + eff) String
body = _.body <$> askAt _request

method ∷ ∀ eff. Run (REQUEST + eff) HTTPure.Method
method = _.method <$> askAt _request

query ∷ ∀ eff. Run (REQUEST + eff) Query
query = parse <$> fullPath
  where
    split = String.Pattern >>> String.split

    parse
      = fromMaybe mempty
      <<<
        ( Query.parse { replacePlus: true }
         <=< Array.last
         <<< split "?"
        )

runRequest ∷
  ∀ a eff.
  HTTPure.Request →
  Run (REQUEST + eff) a →
  Run eff a
runRequest r = runReaderAt _request r

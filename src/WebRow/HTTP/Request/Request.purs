module WebRow.HTTP.Request.Request where

import Prelude

import HTTPure.Method (Method) as HTTPure
import HTTPure.Request (Request) as HTTPure
import Run (Run)
import Run.Reader (READER, askAt, runReaderAt)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type Request r
  = ( request ∷ READER HTTPure.Request | r )

_request = SProxy ∷ SProxy "request"

fullPath ∷ ∀ eff. Run (Request + eff) String
fullPath = _.url <$> askAt _request

body ∷ ∀ eff. Run (Request + eff) String
body = _.body <$> askAt _request

method ∷ ∀ eff. Run (Request + eff) HTTPure.Method
method = _.method <$> askAt _request

runRequest ∷
  ∀ a eff.
  HTTPure.Request →
  Run (Request + eff) a →
  Run eff a
runRequest r = runReaderAt _request r

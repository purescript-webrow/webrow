module WebRow.Contrib.Routing.Duplex where

import Prelude

import Routing.Duplex (RouteDuplex(..), RouteDuplex')
import Routing.Duplex.Parser (RouteParser(..), RouteResult(..))
import Routing.Duplex.Printer (RoutePrinter(..))
import Routing.Duplex.Types (RouteParams)

-- | The rest of query parameters in a
-- | rough format: `Array (Tuple String String)`
-- |
-- | Example usage - extracting rest of the query:
-- |
-- |  route =  D.path "confirmation" $ registrationPasswordQuery
-- |    where
-- |     registrationPasswordQuery = D.record
-- |       # (SProxy ∷ SProxy "email") := (_Newtype <<< D.string $ D.param "email")
-- |       # (SProxy ∷ SProxy "form") := WebRow.Routing.Duplex.params
params ∷ RouteDuplex' RouteParams
params = RouteDuplex printer parser
  where
  parser ∷ RouteParser RouteParams
  parser = Chomp $ \state → Success (state { params = [] }) state.params

  printer ∷ RouteParams → RoutePrinter
  printer p = RoutePrinter \state → state { params = p }


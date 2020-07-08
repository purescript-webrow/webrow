module WebRow.Testing.HTTP.Types where

import WebRow.HTTP (ResponseCookies)

-- | Client are just Response cookies aggregated and pruned
-- | during http session life cycle.
type ClientCookies = ResponseCookies


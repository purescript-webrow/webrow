module WebRow.HTTP.Response.SetHeader where

import Prelude

import Data.Map (insert) as Map
import Data.Newtype (wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import HTTPure (Headers) as HTTPure
import HTTPure.Headers (Headers(..))
import Run (Run)
import Run (lift, on, run, send) as Run

data SetHeaderF a = SetHeader CaseInsensitiveString String a
derive instance functorModifyF ∷ Functor SetHeaderF

type SETHEADER = FProxy SetHeaderF

type SetHeader r = ( setHeader ∷ SETHEADER | r )

_setHeader = SProxy ∷ SProxy "setHeader"

setHeader
  ∷ ∀ eff
  . String
  → String
  → Run ( setHeader ∷ SETHEADER | eff ) Unit
setHeader k v = Run.lift _setHeader (SetHeader (wrap k) v unit)

-- | We want to stay polymorphic over `Response` record here
-- | because during testing we can have different type of the body.
runSetHeader
  ∷ ∀ eff res
  . Run ( setHeader ∷ SETHEADER | eff ) { headers ∷ HTTPure.Headers | res }
  → Run eff { headers ∷ HTTPure.Headers | res }
runSetHeader = Run.run $
  Run.on _setHeader (\(SetHeader k v a) → pure (set k v <$> a)) Run.send
  where
    set k v (r@{ headers: Headers h }) =
      let
        headers = Headers (Map.insert k v h)
      in
        r { headers = headers }



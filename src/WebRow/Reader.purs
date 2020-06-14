module WebRow.Reader where

-- import Prelude

-- import HTTPure (Request) as HTTPure
-- import Run (Run)
-- import Run.Reader (READER) as Run.Reader
-- import Run.Reader (ask)
-- import WebRow.Crypto (Secret)
-- 
-- type READER ctx = Run.Reader.READER
--   { request ∷ HTTPure.Request
--   , secret ∷ Secret | ctx
--   }
-- 
-- request ∷ ∀ ctx eff. Run (reader ∷ READER ctx | eff) HTTPure.Request
-- request = ask <#> _.request
-- 
-- secret ∷ ∀ ctx eff. Run (reader ∷ READER ctx | eff) Secret
-- secret = ask <#> _.secret

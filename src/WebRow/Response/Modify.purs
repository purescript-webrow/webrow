module WebRow.Response.Modify where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import HTTPure as HTTPure
import Run (Run, AFF)
import Run as Run
import Run.Reader (READER)
import WebRow.Cookies as Cookie
import WebRow.Crypto (Secret)

setCookie
  ∷ ∀ ctx eff
  . { name ∷ Cookie.Name, value ∷ Cookie.Value, attributes ∷ Cookie.Attributes }
  → Run
      ( modifyResponse ∷ MODIFY
      , aff ∷ AFF
      , reader ∷ READER { secret ∷ Secret | ctx }
      | eff
      )
      Unit
setCookie = Cookie.setCookie >=> (modifyHeaders <<< const)

modifyHeadersM
  ∷ ∀ eff m
  . Monad m
  ⇒ (HTTPure.Response → m HTTPure.Headers)
  → Run ( modifyResponse ∷ MODIFY_M m | eff ) Unit
modifyHeadersM f = modifyM \res → do
  h ← f res
  pure $ res { headers = h <> res.headers }

modifyHeaders
  ∷ ∀ eff
  . (HTTPure.Response → HTTPure.Headers)
  → Run ( modifyResponse ∷ MODIFY | eff ) Unit
modifyHeaders f = modifyHeadersM (Identity <<< f)

-- | Declare that the resulting response needs to be altered with a given function
modifyM
  ∷ ∀ m eff
  . (HTTPure.Response → m HTTPure.Response)
  → Run ( modifyResponse ∷ MODIFY_M m | eff ) Unit
modifyM f = Run.lift _modifyResponse (Modify f unit)

-- | Simplier version of `modifyM` where the altering function is pure
modify
  ∷ ∀ eff
  . (HTTPure.Response → HTTPure.Response)
  → Run ( modifyResponse ∷ MODIFY | eff ) Unit
modify f = Run.lift _modifyResponse (Modify (Identity <<< f) unit)

-- | Interpret `modifyResponse` Effect by altering the resulting `HTTPure.Response`
run
  ∷ ∀ eff
  . Run ( modifyResponse ∷ MODIFY | eff ) HTTPure.Response
  → Run eff HTTPure.Response
run = Run.runCont
  (Run.on _modifyResponse (\(Modify f a) → un Identity <<< f <$> a) sendFlattened)
  pure

-- | Similar to the `run` function, but the altering function needs to access
-- | the effects from the time of the interpretation
runM
  ∷ ∀ eff
  . Run ( modifyResponse ∷ MODIFY_M (Run eff) | eff ) HTTPure.Response
  → Run eff HTTPure.Response
runM = Run.runCont
  (Run.on _modifyResponse (\(Modify f a) → f =<< a) sendFlattened)
  pure

sendFlattened ∷ ∀ a eff. Run.VariantF eff (Run eff a) → Run eff a
sendFlattened = Run.send >=> identity

-- | Effect for modifying the response that is constructed later in the program
-- |
-- | The altering function can work in some monad `m` and it will be executed
-- | during the interpretation of this effect.
-- |
-- | `m` can be `Identity` is case the context is not needed
-- | (e.g. there's no need to inspect the application state at the time of the interpretation)
data ModifyF m a
  = Modify (HTTPure.Response → m HTTPure.Response) a
derive instance functorModifyF ∷ Functor (ModifyF m)

type MODIFY = FProxy (ModifyF Identity)

type MODIFY_M m = FProxy (ModifyF m)

_modifyResponse = SProxy ∷ SProxy "modifyResponse"

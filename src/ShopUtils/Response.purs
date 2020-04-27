module ShopUtils.Response where

import Prelude

import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), Variant)
import Data.Variant.Internal (FProxy)
import Prim.Row as R
import Run (Run)
import Run as Run

-- | Newtyped Variant that represents possible responses
newtype Response res = Response (Variant res)
derive instance newtypeResponse ∷ Newtype (Response res) _

data ResF res a = Res (Response res)
derive instance functorResponseF ∷ Functor (ResF res)

type RESPONSE res = FProxy (ResF res)

_response = SProxy ∷ SProxy "response"

response ∷
  ∀ res eff a
  . Response res
  → Run ( response ∷ RESPONSE res | eff ) a
response v = Run.lift _response (Res v)

-- | Fully interpret the Response effect returning the Response variant
runResponse
  ∷ ∀ res eff
  . Run ( response ∷ RESPONSE res | eff ) (Response res)
  → Run eff (Response res)
runResponse = runResponseWith (pure <<< pure)

-- | Fully interpret the Response effect with a handler
runResponseWith
  ∷ ∀ a res eff
  . ( Response res
    → Run eff (Run ( response ∷ RESPONSE res | eff ) a)
    )
  → Run ( response ∷ RESPONSE res | eff ) a
  → Run eff a
runResponseWith f = Run.run (Run.on _response (\(Res v) → f v) Run.send)

-- | Allows you to partially interpret Response effect. This function lets you
-- | inspect the `Response res` and make changes to it.
onResponse
  ∷ ∀ res res' eff a
  . R.Union eff ( response ∷ RESPONSE res' ) ( response ∷ RESPONSE res' | eff )
  ⇒ ( ∀ b
    . Response res
    → Run ( response ∷ RESPONSE res' | eff ) b
    )
  → Run ( response ∷ RESPONSE res  | eff ) a
  → Run ( response ∷ RESPONSE res' | eff ) a
onResponse = onResponse'

-- | Less restrictive version of `onResponse`,
-- | letting you specify the rest of the program i.e. resume
onResponse'
  ∷ ∀ res res' eff a
  . R.Union eff ( response ∷ RESPONSE res' ) ( response ∷ RESPONSE res' | eff )
  ⇒ ( Response res
    → Run ( response ∷ RESPONSE res' | eff )
        (Run ( response ∷ RESPONSE res | eff ) a)
    )
  → Run ( response ∷ RESPONSE res  | eff ) a
  → Run ( response ∷ RESPONSE res' | eff ) a
onResponse' f = Run.run (Run.on _response (\(Res v) → f v) (Run.send >>> Run.expand))

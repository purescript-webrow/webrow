module ShopUtils.Response where

import Prelude

import Data.Variant (SProxy(..), Variant)
import Data.Variant.Internal (FProxy)
import Prim.Row as R
import Run (Run)
import Run as Run

data ResF res a = Res (Variant res)

derive instance functorResponseF ∷ Functor (ResF res)

type RES res = FProxy (ResF res)

_res = SProxy ∷ SProxy "res"

response ∷
  ∀ res eff a
  . Variant res
  → Run ( res ∷ RES res | eff ) a
response v = Run.lift _res (Res v)

-- | Fully interpret the Response effect
runResponse
  ∷ ∀ a res eff
  . ( Variant res
    → Run eff (Run ( res ∷ RES res | eff ) a)
    )
  → Run ( res ∷ RES res | eff ) a
  → Run eff a
runResponse f = Run.run (Run.on _res (\(Res v) → f v) Run.send)

-- | Allows you to partially interpret Response effect. This function lets you
-- | inspect the `Variant res` and make changes to it.
onResponse
  ∷ ∀ res res' eff a
  . R.Union eff ( res ∷ RES res' ) ( res ∷ RES res' | eff )
  ⇒ ( ∀ b
    . Variant res
    → Run ( res ∷ RES res' | eff ) b
    )
  → Run ( res ∷ RES res  | eff ) a
  → Run ( res ∷ RES res' | eff ) a
onResponse = onResponse'

-- | Less restrictive version of `onResponse`,
-- | letting you specify the rest of the program i.e. resume
onResponse'
  ∷ ∀ res res' eff a
  . R.Union eff ( res ∷ RES res' ) ( res ∷ RES res' | eff )
  ⇒ ( Variant res
    → Run ( res ∷ RES res' | eff )
        (Run ( res ∷ RES res | eff ) a)
    )
  → Run ( res ∷ RES res  | eff ) a
  → Run ( res ∷ RES res' | eff ) a
onResponse' f = Run.run (Run.on _res (\(Res v) → f v) (Run.send >>> Run.expand))

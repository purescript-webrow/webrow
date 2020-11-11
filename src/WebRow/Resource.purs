module WebRow.Resource where

import Prelude
import Control.Monad.Resource (Resource) as Resource
import Control.Monad.Resource (runResource)
import Control.Monad.Trans.Class (lift) as Trans.Class
import Data.Functor.Variant (on)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Run (FProxy, Run)
import Run (lift, match, run, send) as Run
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Run (AffRow, EffRow, _aff, _effect)

type RESOURCE
  = FProxy Resource.Resource

_resource = SProxy ∷ SProxy "resource"

type Resource r
  = ( resource ∷ RESOURCE | r )

liftAff ∷ ∀ r. Aff ~> Run (Resource + r)
liftAff = Run.lift _resource <<< Trans.Class.lift

liftResource ∷ ∀ r. Resource.Resource ~> Run (Resource + r)
liftResource = Run.lift _resource

liftEffect ∷ ∀ r. Effect ~> Run (Resource + r)
liftEffect = Run.lift _resource <<< Effect.liftEffect

liftBaseAff ∷ ∀ r. Run (AffRow + EffRow + Resource + r) ~> Run (Resource + r)
liftBaseAff = Run.run handleBaseAff
  where
  handleBaseAff =
    Run.send
      # on _aff liftAff
      # on _effect (liftAff <<< Effect.liftEffect)

run ∷ Run (Resource + ()) ~> Aff
run = runResource <<< Run.run (Run.match { resource: \a → a })

runBaseResource ∷ Run (Resource + ()) ~> Aff
runBaseResource r = runResource $ Run.run m r
  where
  m = Run.match { resource: \a → a }

runBaseResource' ∷ Run (AffRow + EffRow + Resource + ()) ~> Aff
runBaseResource' r = runResource $ Run.run m r
  where
  m =
    Run.match
      { aff: \a → Trans.Class.lift a
      , effect: \a → Trans.Class.lift $ Effect.liftEffect a
      , resource: \a → a
      }

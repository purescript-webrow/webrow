module WebRow.Resource where

import Prelude

import Control.Monad.Resource (Resource)
import Control.Monad.Resource (runResource)
import Control.Monad.Trans.Class (lift) as Trans.Class
import Data.Functor.Variant (on)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as Effect
import Run (AFF, Run, EFFECT)
import Run (lift, match, run, send) as Run
import Type.Prelude (SProxy(..))
import Type.Row (type (+))
import WebRow.Contrib.Run (_aff, _effect)

_resource = SProxy ∷ SProxy "resource"

type RESOURCE r
  = ( resource ∷ Resource | r )

liftAff ∷ ∀ r. Aff ~> Run (RESOURCE + r)
liftAff = Run.lift _resource <<< Trans.Class.lift

liftResource ∷ ∀ r. Resource ~> Run (RESOURCE + r)
liftResource = Run.lift _resource

liftEffect ∷ ∀ r. Effect ~> Run (RESOURCE + r)
liftEffect = Run.lift _resource <<< Effect.liftEffect

liftBaseAff ∷ ∀ r. Run (AFF + EFFECT + RESOURCE + r) ~> Run (RESOURCE + r)
liftBaseAff = Run.run handleBaseAff
  where
  handleBaseAff =
    Run.send
      # on _aff liftAff
      # on _effect (liftAff <<< Effect.liftEffect)

run ∷ Run (RESOURCE + ()) ~> Aff
run = runResource <<< Run.run (Run.match { resource: \a → a })

runBaseResource ∷ Run (RESOURCE + ()) ~> Aff
runBaseResource r = runResource $ Run.run m r
  where
  m = Run.match { resource: \a → a }

runBaseResource' ∷ Run (AFF + EFFECT + RESOURCE + ()) ~> Aff
runBaseResource' r = runResource $ Run.run m r
  where
  m =
    Run.match
      { aff: \a → Trans.Class.lift a
      , effect: \a → Trans.Class.lift $ Effect.liftEffect a
      , resource: \a → a
      }

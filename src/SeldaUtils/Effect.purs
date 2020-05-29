module SeldaUtils.Effect where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Either (either)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Data.Variant.Internal (FProxy)
import Database.PostgreSQL (Connection, PGError)
import Effect.Aff (Aff)
import Run (Run, AFF)
import Run as Run
import Run.Except (Except(..), EXCEPT)
import Run.Except as Run.Except
import Run.Reader (READER)
import Run.Reader as Run.Reader
import Selda.Query.Class (runSelda)
import SeldaUtils.Class (SeldaPG)

type SELDA = FProxy SeldaPG

type SeldaEff eff = ( selda ∷ SELDA | eff )

_selda = SProxy ∷ SProxy "selda"

liftSelda ∷ ∀ eff. SeldaPG ~> Run ( selda ∷ SELDA | eff )
liftSelda m = Run.lift _selda m

interpretPG
  ∷ ∀ e r eff a
  . Run ( selda ∷ SELDA | Effects PGError Connection e r eff ) a
  → Run (               | Effects PGError Connection e r eff ) a
interpretPG = Run.interpret (Run.on _selda hoistSelda Run.send)

hoistSelda
  ∷ ∀ dbErr dbConn e r eff
  . ExceptT dbErr (ReaderT dbConn Aff)
  ~>Run (Effects dbErr dbConn e r eff)
hoistSelda m = do
  conn ← Run.Reader.ask <#> _.dbConn
  ea ← Run.liftAff $ runSelda conn m
  either (Run.Except.liftExcept <<< Except <<< inj _selda) pure ea

type Effects dbErr dbConn e r eff = 
  ( aff ∷ AFF
  , except ∷ EXCEPT (Variant ( selda ∷ dbErr | e ))
  , reader ∷ READER { dbConn ∷ dbConn | r }
  | eff
  )

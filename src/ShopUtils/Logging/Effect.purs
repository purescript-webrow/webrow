module ShopUtils.Logging.Effect where

import Prelude

import Control.Logger.Journald (Level(..))
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Effect.Class.Console as Console
import Run (Run, AFF)
import Run as Run

data LoggerF a
  = Log Level String a

derive instance functorLoggerF ∷ Functor LoggerF

type LOGGER = FProxy LoggerF

_logger = SProxy ∷ SProxy "logger"

log ∷ ∀ eff. Level → String → Run ( logger ∷ LOGGER | eff ) Unit
log lvl msg = Run.lift _logger (Log lvl msg unit)

info ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
info = log Info

warning ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
warning = log Warning

err ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
err = log Err

runLoggerConsole
  ∷ ∀ a eff
  . Run ( aff ∷ AFF , logger ∷ LOGGER | eff ) a
  → Run ( aff ∷ AFF                   | eff ) a
runLoggerConsole = Run.interpret (Run.on _logger handleLoggerConsole Run.send)

handleLoggerConsole
  ∷ ∀ a eff
  . LoggerF a
  → Run ( aff ∷ AFF | eff ) a
handleLoggerConsole (Log lvl msg next) = do
  Run.liftAff $ Console.log $ (show lvl) <> ": " <> show msg
  pure next

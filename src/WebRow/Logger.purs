module WebRow.Logger
  ( debug
  , err
  , info
  , LOGGER
  , Logger
  , runToConsole
  , warning
  , LoggerF(..)
  , module Level
  ) where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Effect.Class.Console as Console
import Run (Run, AFF)
import Run as Run
import WebRow.Logger.Level (Level(..))
import WebRow.Logger.Level (Level(..)) as Level

data LoggerF a = LoggerF Level String a

derive instance functorLoggerF ∷ Functor LoggerF

type LOGGER
  = FProxy LoggerF

type Logger r = (logger ∷ LOGGER | r)

_logger = SProxy ∷ SProxy "logger"

log ∷ ∀ eff. Level → String → Run ( logger ∷ LOGGER | eff ) Unit
log lvl msg = Run.lift _logger (LoggerF lvl msg unit)

debug ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
debug = log Debug

info ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
info = log Info

warning ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
warning = log Warning

err ∷ ∀ eff. String → Run ( logger ∷ LOGGER | eff ) Unit
err = log Err

runToConsole ∷
  ∀ a eff.
  Run ( aff ∷ AFF, logger ∷ LOGGER | eff ) a →
  Run ( aff ∷ AFF | eff ) a
runToConsole = Run.interpret (Run.on _logger handleLoggerConsole Run.send)
  where
  handleLoggerConsole ∷
    ∀ b.
    LoggerF b →
    Run ( aff ∷ AFF | eff ) b
  handleLoggerConsole (LoggerF lvl msg next) = do
    Run.liftAff $ Console.log $ (show lvl) <> ":> " <> show msg
    pure next

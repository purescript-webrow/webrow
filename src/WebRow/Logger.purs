module WebRow.Logger
  ( debug
  , err
  , info
  , LOGGER
  , LOGGER
  , runToConsole
  , warning
  , Logger(..)
  , module Level
  ) where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class.Console as Console
import Run (Run, AFF)
import Run as Run
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import WebRow.Logger.Level (Level(..))
import WebRow.Logger.Level (Level(..)) as Level

data Logger a = Logger Level String a

derive instance functorLogger ∷ Functor Logger

type LOGGER r = (logger ∷ Logger | r)

_logger = SProxy ∷ SProxy "logger"

log ∷ ∀ eff. Level → String → Run ( LOGGER + eff ) Unit
log lvl msg = Run.lift _logger (Logger lvl msg unit)

debug ∷ ∀ eff. String → Run ( LOGGER + eff ) Unit
debug = log Debug

info ∷ ∀ eff. String → Run ( LOGGER + eff ) Unit
info = log Info

warning ∷ ∀ eff. String → Run ( LOGGER + eff ) Unit
warning = log Warning

err ∷ ∀ eff. String → Run ( LOGGER + eff ) Unit
err = log Err

runToConsole ∷
  ∀ a eff.
  Run ( AFF + LOGGER + eff ) a →
  Run ( AFF + eff ) a
runToConsole = Run.interpret (Run.on _logger handleLoggerConsole Run.send)
  where
  handleLoggerConsole ∷
    ∀ b.
    Logger b →
    Run ( AFF + eff ) b
  handleLoggerConsole (Logger lvl msg next) = do
    Run.liftAff $ Console.log $ (show lvl) <> ":> " <> show msg
    pure next

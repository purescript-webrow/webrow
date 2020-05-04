module WebRow.Logging where

import Prelude

import Control.Logger (cfilter)
import Control.Logger (cfilter, log, Logger) as Logger
import Control.Logger.Console (console) as Logger
import Control.Logger.Journald (logger') as Journald
import Control.Logger.Journald.Types (Level(..))
import Control.Monad.Reader (class MonadReader, asks)
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Systemd.Journald (journald) as Node.System.Journald
import Record (insert)
import Type.Prelude (SProxy(..))

type Config =
  { console ∷ Maybe Level
  , journald ∷ Maybe Level
  -- , sentry ∷ Maybe
  --     { minLevel ∷ Level
  --     , dsn ∷ String
  --     }
  }

logger
  ∷ Config
  → Effect Logger
logger config = do
  let s = mempty
  j ← for config.journald  \minLevel → do
    journald ← Journald.logger' (Node.System.Journald.journald {})
    pure $
      filterLevel'
        minLevel
        (cmap (insert (SProxy ∷ SProxy "fields") {}) journald)
  pure $ maybe mempty identity (s <> j <> c)
  where
    filterLevel' minLevel = cfilter (\r → r.level >= minLevel)
    c = config.console <#> \minLevel → filterLevel' minLevel (Logger.console _.message)

defaultConfig ∷ Config
defaultConfig =
  { console: Just Debug
  , journald: Nothing
  -- , sentry: Nothing
  }

-- | Let's reuse levels from journald logger for simplicity.
-- |
-- | Emerg - System is unusable. Severe Kernel BUG, systemd dumped core. This level should not be used by applications.
-- | Alert - Should be corrected immediately. Vital subsystem goes out of work.
-- | Critical conditions. Failure in the system primary application
-- | Error - Error conditions. Not severe error reported.
-- | Warning - May indicate that an error will occur if action is not taken.
-- | Notice - Events that are unusual, but not error conditions.
-- | Info - Normal operational messages that require no action.
-- | Debug - Information useful to developers for debugging the application.
type Logger = Logger.Logger Effect Log
type Log = { message ∷ String, level ∷ Level }

filterLevel ∷ Level → Logger → Logger
filterLevel minLevel = Logger.cfilter (\r → r.level >= minLevel)

log ∷ ∀ m. MonadEffect m ⇒ Logger → Log → m Unit
log l m = liftEffect (Logger.log l m)

debug ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
debug m l = log l { level: Debug, message: m }

info ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
info m l = log l { level: Info, message: m }

notice ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
notice m l = log l { level: Notice, message: m }

warning ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
warning m l = log l { level: Warning, message: m }

error ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
error m l = log l { level: Err, message: m }

critical ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
critical m l = log l { level: Crit, message: m }

alert ∷ ∀ m. MonadEffect m ⇒ String → Logger → m Unit
alert m l = log l { level: Alert, message: m }

debugM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
debugM msg = asks _.logger >>= debug msg >>> liftEffect

infoM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
infoM msg = asks _.logger >>= info msg >>> liftEffect

noticeM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
noticeM msg = asks _.logger >>= notice msg >>> liftEffect

warningM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
warningM msg = asks _.logger >>= warning msg >>> liftEffect

errorM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
errorM msg = asks _.logger >>= error msg >>> liftEffect

criticalM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
criticalM msg = asks _.logger >>= critical msg >>> liftEffect

alertM ∷ ∀ ctx m. MonadEffect m ⇒ MonadReader { logger ∷ Logger | ctx } m ⇒ String → m Unit
alertM msg = asks _.logger >>= alert msg >>> liftEffect

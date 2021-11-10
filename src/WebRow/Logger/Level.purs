module WebRow.Logger.Level where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))

-- | TODO: extract purescript-logging-level
-- | because these names are taken from journald lib.
data Level
  = Debug
  | Info
  | Notice
  | Warning
  | Err
  | Crit
  | Alert
  | Emerg

derive instance genericLevel ∷ Generic Level _

derive instance ordLevel ∷ Ord Level

derive instance eqLevel ∷ Eq Level

instance showLevel ∷ Show Level where
  show = genericShow

fromString ∷ String → Maybe Level
fromString "DEBUG" = Just Debug

fromString "INFO" = Just Info

fromString "NOTICE" = Just Notice

fromString "WARNING" = Just Warning

fromString "ERR" = Just Err

fromString "CRIT" = Just Crit

fromString "ALERT" = Just Alert

fromString "EMERG" = Just Emerg

fromString _ = Nothing

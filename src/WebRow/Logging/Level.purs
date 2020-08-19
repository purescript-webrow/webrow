module WebRow.Logging.Level where

import Data.Maybe (Maybe(..))

data Level
  = Debug
  | Info
  | Notice
  | Warning
  | Err
  | Crit
  | Alert
  | Emerg

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

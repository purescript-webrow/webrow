module WebRow.Auth.Interpret where

import WebRow.Applets.Auth.Effects (User) as Effects

type User
  = Effects.User ()

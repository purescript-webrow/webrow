module WebRow.Applets.Auth.Messages where

import Type.Row (type (+))
import WebRow.Applets.Auth.Forms (AuthFailed)

type Messages r = AuthFailed + r

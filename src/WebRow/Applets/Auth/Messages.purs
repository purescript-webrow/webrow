module WebRow.Applets.Auth.Messages where

import Polyform.Batteries.UrlEncoded.Validators (SingleValueExpected)
import Type.Row (type (+))
import WebRow.Applets.Auth.Forms (AuthFailed)
import WebRow.Forms.Validators (InvalidEmailFormat)


type Messages r = AuthFailed + InvalidEmailFormat + SingleValueExpected + r

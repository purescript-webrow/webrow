module WebRow.Applets.Registration.Messages where

import Type.Row (type (+))
import WebRow.Applets.Registration.Forms (EmailTaken, PasswordsMismatch)

type Messages r = EmailTaken + PasswordsMismatch + r

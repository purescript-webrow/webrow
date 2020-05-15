module WebRow.Applets.Auth.Forms where

import Prelude

-- import WebRow.Applets.Registration.Forms (emailFormat, nonEmptyString)
-- import WebRow.Forms.Builders.Plain (passwordField)
-- import WebRow.Forms.Builders.Plain as Forms.Builders.Plain
-- 
-- emailPassordForm = { email: _, password: _ } <$> emailForm <*> passwordForm
--   where
--   passwordForm = passwordField "password" nonEmptyString
--   emailForm = Forms.Builders.Plain.field
--     { name: "email", type_: "email" }
--     (nonEmptyString >>> emailFormat)

module WebRow.Auth.Interpret where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import Run (Run(..))
import Selda (Col, lit, restrict, selectFrom, (.==))
import Selda as Selda
import Selda.PG.Class (class MonadSeldaPG, BackendPGClass, query1)
import Selda.Query (class FromTable)
import Selda.Query.Class (class GenericQuery)
import SeldaUtils (exec)
import SeldaUtils.Effect (SELDA, liftSelda)
import WebRow.Applets.Auth.Types (Password(..))
import WebRow.Crypto as Crypto
import WebRow.Mailer (Email(..))

-- interpret
--   ∷ ∀ eff
--   . Run
--     ( register ∷ REGISTER
--     | eff
--     )
--    ~> Run (effect ∷ EFFECT | eff)
-- interpret = Run.interpret (Run.on _register handler Run.send)

-- handler
  -- ∷ ∀ eff
  -- . RegisterF ~> Run (effect ∷ EFFECT | eff)
-- handler (Authenticate email password k) = do
--   selectFrom usersTable
--   v ← liftEffect random
--   pure (next (v > 0.5))

type UserRow r = ( email ∷ String, password ∷ String, salt ∷ String | r )

usersTable ∷ Selda.Table (UserRow ())
usersTable = Selda.Table { name: "users" }

-- | Create a basic default table "users" with columns: email, password, salt
createUsersTable ∷ ∀ eff. Run ( selda ∷ SELDA | eff ) Unit
createUsersTable = liftSelda $ exec
  """
  CREATE TABLE IF NOT EXISTS users (
    email text PRIMARY KEY,
    password text NOT NULL,
    salt text NOT NULL
  );
  """

authenticate
  ∷ ∀ m
  . MonadSeldaPG m
  ⇒ Email
  → Password
  → m (Maybe Unit)
authenticate (Email email) (Password password) = do
  getUserByEmail usersTable email >>= maybe
    (pure Nothing)
    \user → do
      hashedPass ← liftEffect $ Crypto.hash $ password <> user.salt
      pure $ if user.password == hashedPass
        then Just unit
        else Nothing

getUserByEmail
  ∷ ∀ t m i o
  . MonadSeldaPG m
  ⇒ FromTable Unit t ( email ∷ Col Unit String | i )
  ⇒ GenericQuery BackendPGClass m 
      ( email ∷ Col Unit String | i )
      ( email ∷ String | o )
  ⇒ Selda.Table t → String → m (Maybe { email ∷ String | o })
getUserByEmail table email = query1 q
  where
    q = selectFrom table \r → do
      restrict $ r.email .== lit email
      pure r
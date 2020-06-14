module WebRow.Selda where

import Prelude

import Control.Monad.Reader (ask)
import Data.Either (either)
import Data.Maybe (Maybe, maybe)
import Database.PostgreSQL (class FromSQLRow, PGError, Row0(..))
import Database.PostgreSQL as PG
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Selda.PG.Class (class MonadSeldaPG)

pgExecute ∷ PG.Connection → String → Aff (Maybe PGError)
pgExecute conn q = PG.execute conn (PG.Query q) Row0

exec ∷ ∀ m. MonadSeldaPG m ⇒ String → m Unit
exec q = do
  conn ← ask
  merr ← liftAff $ pgExecute conn q
  maybe (pure unit) throwError merr

queryPG ∷ ∀ m a. MonadSeldaPG m ⇒ FromSQLRow a ⇒ String → m (Array a)
queryPG q = do
  conn ← ask
  (liftAff $ PG.query conn (PG.Query q) Row0)
    >>= either throwError pure

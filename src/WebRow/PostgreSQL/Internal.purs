module WebRow.PostgreSQL.Internal where

import Prelude
import Control.Monad.Resource (Resource) as Resource
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant.Internal (FProxy)
import Database.PostgreSQL (Connection, PGError, Pool, fromPool) as PG
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff) as Aff.Class
import Effect.Class (liftEffect) as Effect.Class
import Run (Run)
import Run as Run
import Type.Row (type (+))

-- | We prevent nesting transaction on the type level.
foreign import kind TransactionMode

foreign import data Inside ∷ TransactionMode

foreign import data Outside ∷ TransactionMode

type PG mode
  = FProxy (PGF mode)

_pg = SProxy ∷ SProxy "pg"

type Pg mode r
  = ( pg ∷ PG mode | r )

-- | TODO:
-- | This is somewhat unsafe meaning we can have
-- | (mode ∷ Inside) but lack the connection value...
-- | Should we really care about this incosistency?
newtype Conn (mode ∷ TransactionMode)
  = Conn (PG.Pool /\ (Maybe PG.Connection))

-- | I'm not sure if it is possible to abstract away
-- | whole pg interaction if I want to preserve
-- | `withTransaction` (it seems that it should contain
-- | `Run` value inside itself - is it possible?).
-- | Because of this problem we provide only tiny a layer
-- | above reader / Aff effect (to limit the damage).
newtype PGF mode a
  = PGF (Conn mode → Resource.Resource (Either PG.PGError a))

derive instance pgFunctor ∷ Functor (PGF mode)

connection ∷ ∀ mode r. Run (Pg mode + r) PG.Connection
connection = Run.lift _pg (PGF (pure <<< wrap))
  where
  wrap (Conn (p /\ Nothing)) = Right (PG.fromPool p)

  wrap (Conn (_ /\ (Just conn))) = Right conn

pool ∷ ∀ mode r. Run (Pg mode + r) PG.Pool
pool = Run.lift _pg (PGF (pure <<< wrap))
  where
  wrap (Conn (p /\ _)) = Right p

liftPGAff ∷ ∀ a mode r. Aff (Either PG.PGError a) → Run (Pg mode + r) a
liftPGAff action = Run.lift _pg (PGF $ const $ Aff.Class.liftAff action)

liftAff ∷ ∀ a mode r. Aff a → Run (Pg mode + r) a
liftAff action = liftPGAff (map Right action)

liftEffect ∷ ∀ a mode r. Effect a → Run (Pg mode + r) a
liftEffect action = liftAff $ Effect.Class.liftEffect action


module ShopUtils.Types where

import Prelude

import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (asks, withReaderT)
import Control.Monad.Reader.Trans (ReaderT(..), mapReaderT, runReaderT)
import Control.Monad.State (StateT(..), evalStateT, gets, runStateT)
import Control.Monad.State.Trans (StateT)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Database.PostgreSQL (Connection, PoolConfiguration, newPool, withConnection, withTransaction) as PostgreSQL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global.Unsafe (unsafeStringify)
import HTTPure (Request, Response, ResponseM, internalServerError) as HTTPure
import Prim.Row (class Cons, class Lacks) as Row
import Record (delete, insert, set) as Record
import ShopUtils.Logging (logger)
import ShopUtils.Logging as Logging
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type AppM e ctx st =
  ExceptT (Variant ( | e ))
    (ReaderT { | ctx }
      (StateT { | st } Aff))

type App e ctx st = AppM e ctx st HTTPure.Response

hoistWith
  ∷ ∀ e' e r' s' s m r
  . Monad m
  ⇒ (e → e')
  → (r' → r)
  → (s → m s') → (s' → m s)
  → ExceptT e (ReaderT r (StateT s m))
  ~> ExceptT e' (ReaderT r' (StateT s' m))
hoistWith fe fr fs fs' m = withExceptT fe $ ExceptT $ ReaderT \ctx → 
  withStateT' fs' fs (runReaderT (runExceptT m) $ fr ctx)

hoistReader fr = hoistWith identity fr pure pure

hoistState = hoistWith identity identity

-- type RequestCtx ctx = ( request ∷ HTTPure.Request | ctx )

runApp
  ∷ ∀ e ctx st
  . Row.Lacks "request" ctx
  ⇒ { | ctx }
  → { | st }
  → App e ( request ∷ HTTPure.Request | ctx ) st
  → HTTPure.Request
  → Aff (Either (Variant ( | e )) HTTPure.Response)
runApp ctx st app request = evalStateT (runReaderT (runExceptT app) ctx') st
  where
    ctx' = Record.insert (SProxy ∷ SProxy "request") request ctx

type Middleware e ctx st e' ctx' st' = App e ctx st → App e' ctx' st'
type Middleware' e ctx st = App e ctx st → App e ctx st

withStateT' ∷ ∀ a m s s'. Monad m ⇒ (s' → m s) → (s → m s') → StateT s m a → StateT s' m a
withStateT' f t (StateT m) = StateT (\s → f s >>= m >>= traverse t)

withReaderT' ∷ ∀ a m r r'. Monad m ⇒ (r' → m r) → ReaderT r m a → ReaderT r' m a
withReaderT' f (ReaderT m) = ReaderT (\r → f r >>= m)

withReaderT'' ∷ ∀ a e m r r'. Monad m ⇒ (r' → m r) → (Either e a → m a) → ReaderT r m (Either e a) → ReaderT r' m a
withReaderT'' f h (ReaderT m) = ReaderT (\r → f r >>= m >>= h)

affMiddleware
  ∷ ∀ e ctx st
  . { onRequest ∷ AppM e ctx st Unit, onResponse ∷ HTTPure.Response → AppM e ctx st Unit }
  → Middleware' e ctx st
affMiddleware { onRequest, onResponse } app = do
  onRequest
  response ← app
  onResponse response
  pure response

addConstMiddleware
  ∷ ∀ e ctx ctx' st s v
  . IsSymbol s
  ⇒ Row.Lacks s ctx
  ⇒ Row.Cons s v ctx ctx'
  ⇒ SProxy s
  → AppM e ctx st v
  → Middleware e ctx' st e ctx st
addConstMiddleware p v app = do
  v' ← v
  hoistReader (Record.insert p v') app

type LoggerCtx ctx = ( logger ∷ Logging.Logger | ctx )

type LoggerMiddleware e ctx st = Middleware
  e ( LoggerCtx + ctx ) st
  e ( logger ∷ Logging.Config | ctx ) st

loggerMiddleware
  ∷ ∀ e ctx st
  . AppM e (logger ∷ Logging.Config | ctx) st Logging.Logger
  → LoggerMiddleware e ctx st
loggerMiddleware buildLogger app = do
  lg ← buildLogger
  let withLogger = Record.set (SProxy ∷ SProxy "logger") lg
  hoistReader withLogger app

loggerMiddleware'
  ∷ ∀ e ctx st
  . Middleware
      e ( logger ∷ Logging.Logger | ctx ) st
      e ( logger ∷ Logging.Config | ctx ) st
loggerMiddleware' app = do
  lg ← asks _.logger >>= \cfg → liftEffect (logger cfg)
  let withLogger = Record.set (SProxy ∷ SProxy "logger") lg
  hoistReader withLogger app

type SessionSt session st = ( session ∷ session | st )

type SessionMiddleware session e ctx st = Middleware
  e ctx (SessionSt session + st)
  e ctx st

sessionMiddleware ∷
  ∀ e ctx session st
  . Row.Lacks "session" st
  ⇒ AppM e ctx st session
  → ({ response ∷ HTTPure.Response, session ∷ session } → App e ctx st)
  → SessionMiddleware session e ctx st
sessionMiddleware getSession setSession app = app'
  where
    withSession session = hoistState
      (\st → pure $ Record.delete (SProxy ∷ SProxy "session") st)
      (\st → pure $ Record.insert (SProxy ∷ SProxy "session") session st)
    app' = do
      r ← getSession >>= flip withSession do
        response ← app
        session ← gets _.session
        pure { response, session }
      setSession r

type DbConnectionCtx ctx = ( conn ∷ PostgreSQL.Connection | ctx )

type DBMiddleware e ctx st = Middleware
  e ( conn ∷ PostgreSQL.Connection | ctx ) st
  e ( db ∷ PostgreSQL.PoolConfiguration | ctx ) st

-- dbMiddleware ∷ ∀ e ctx st. DBMiddleware e (LoggerCtx ctx) st
-- dbMiddleware app = do
--   poolConfiguration ← asks _.db
--   pool ← liftEffect $ PostgreSQL.newPool poolConfiguration
--   ReaderT \ctx → StateT \st →
--     PostgreSQL.withConnection pool case _ of
--       Right conn → do
--         let ctx' = Record.set (SProxy ∷ SProxy "conn") conn ctx
--         runStateT (runReaderT app ctx') st
--       Left e → internalServerError e ctx.logger <#> (flip Tuple st)

-- type DbTransactionCtx ctx = DbConnectionCtx + (inTransaction ∷ Unit | ctx)

-- type DBTransactionMiddleware ctx st = Middleware
--   (DbTransactionCtx + ctx) st
--   (DbConnectionCtx + ctx) st

-- dbTransactionMiddleware ∷ ∀ ctx st. Row.Lacks "inTransaction" ctx ⇒ DBTransactionMiddleware ctx st
-- dbTransactionMiddleware app = ReaderT \ctx → StateT \st → do
--     let
--       ctx' = Record.insert (SProxy ∷ SProxy "inTransaction") unit ctx
--     result ← PostgreSQL.withTransaction ctx.conn (runStateT (runReaderT app ctx') st)
--     case result of
--       Right t → pure t
--       Left e → internalServerError e ctx.logger <#> (flip Tuple st)

-- | XXX: Provide proper error handling appropriate for pwa and brower request
-- internalServerError ∷ ∀ e. e → Logging.Logger → HTTPure.ResponseM
-- internalServerError e logger = do
--   liftEffect $ Logging.error (unsafeStringify e) logger
--   HTTPure.internalServerError "Server error..."

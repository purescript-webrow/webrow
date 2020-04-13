module SeldaUtils.CLI where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), optional)
import Database.PostgreSQL (defaultPoolConfiguration)
import Database.PostgreSQL as PG
import Effect (Effect)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse

type Options =
  { database ∷ PG.PoolConfiguration
  -- , schema ∷ String
  -- , action ∷ Action
  }

getOptions ∷ Effect Options
getOptions = Optparse.execParser (Optparse.info (options <**> Optparse.helper) Optparse.fullDesc)

options ∷ Optparse.Parser Options
options =
  { database: _
  -- , schema: _
  -- , action: _
  }
  <$> database
  -- <*> schema
  -- <*> action
  where
    database = pool <$> (db <|> pure default)
      where
        pool cfg = (defaultPoolConfiguration cfg.name)
          { host = cfg.host
          , idleTimeoutMillis = cfg.idleTimeoutMillis
          , user = cfg.user
          , password = cfg.password
          , port = cfg.port
          }

        default =
          { host: Nothing
          , idleTimeoutMillis: Just 100
          , name: "streaming_stats"
          , password: Just "qwerty"
          , port: Nothing
          , user: Just "init"
          }

        db = { host: _, idleTimeoutMillis: _, name: _, password: _, port: _, user: _ }
          <$> optional host
          <*> (Just <$> idleTimeoutMillis <|> pure default.idleTimeoutMillis)
          <*> name
          <*> optional password
          <*> optional port
          <*> optional user

        host = Optparse.strOption $
          Optparse.long "db-host"
          <> Optparse.metavar "DATABASE_HOST"
          <> Optparse.help "Database host"

        idleTimeoutMillis = (Optparse.option Optparse.int) $
          Optparse.long "db-idle-timeout"
          <> Optparse.metavar "TIMOUT_IN_MILLISECONDS"
          <> Optparse.help "Database connection idle timeout. Useful when testing."

        name = Optparse.strOption $
          Optparse.long "db-name"
          <> Optparse.metavar "DATABASE"
          <> Optparse.help "Database name"

        password = Optparse.strOption $
          Optparse.long "db-password"
          <> Optparse.metavar "DATABASE_PASSWORD"
          <> Optparse.help "Database password"

        port = (Optparse.option Optparse.int) $
          Optparse.long "db-port"
          <> Optparse.metavar "DATABASE_PORT"
          <> Optparse.help "Database port"

        user = Optparse.strOption $
          Optparse.long "db-user"
          <> Optparse.metavar "DATABASE_USER"
          <> Optparse.help "Database user"

    schema = Optparse.strOption $
      Optparse.long "db-schema"
      <> Optparse.short 's'
      <> Optparse.metavar "SCHEMA"
      <> Optparse.help "Database schema"

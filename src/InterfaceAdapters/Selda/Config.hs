{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module InterfaceAdapters.Selda.Config where


import Data.Pool (Pool, createPool)

import Database.Selda.Backend (SeldaConnection, seldaClose)
import Database.Selda.PostgreSQL as SPG (PGConnectInfo(..), PG, pgOpen, pgHost, pgPort, pgDatabase, pgSchema, pgPassword, pgUsername)

import Dhall (FromDhall, input, auto, Text, Generic)


data DbConnPool = DbConnPool {
  getDbConnPool :: Pool (SeldaConnection PG)
}


-- | This creates a DbConfig object containing a created db conn POOL.
-- Also invoked from GHCi, when running/testing queries manually
mkDbConfig :: IO DbConnPool
mkDbConfig = do
  pool <- makePgConnPoolSelda
  return $ DbConnPool pool


-- | Setting the DB config info, which should be read from a config file in the future
makePgConnPoolSelda :: IO (Pool (SeldaConnection PG))
makePgConnPoolSelda = do
  dbconninfo <- pgConnectInfo
  makePgConnPoolFromInfoSelda dbconninfo


data EnvUserConfig = EnvUserConfig {
    env :: Text, user :: Text
} deriving (Generic, Show)
instance FromDhall EnvUserConfig


pgConnectInfo :: IO PGConnectInfo
pgConnectInfo = do
  envuser <- input auto "./config/config.txt"
  let connInfo = SPG.PGConnectInfo { SPG.pgHost="localhost"
                                      ,SPG.pgPort=5432
                                      ,SPG.pgDatabase= "" -- init; to be set later
                                      ,SPG.pgSchema = Nothing -- init; can be set later
                                      ,SPG.pgPassword = Nothing
                                      ,SPG.pgUsername = Nothing -- init; to be set later
                                    }
  return $ connInfo {SPG.pgDatabase = "cleandb",
                     SPG.pgSchema = Just "public",
                     SPG.pgUsername = Just (user envuser)
                    }
  -- 'public' is the default schema in Postgres

makePgConnPoolFromInfoSelda :: SPG.PGConnectInfo -> IO (Pool (SeldaConnection PG))
makePgConnPoolFromInfoSelda connectionInfo = createPool (SPG.pgOpen connectionInfo) seldaClose 4 2 10

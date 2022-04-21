{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module InterfaceAdapters.KVSSelda where

import           Polysemy

import           Data.Aeson                     (decode, encode)
import           Data.Aeson.Types               (FromJSON, ToJSON)
import           Data.Text (unpack)
import qualified Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Data.Pool (Pool, createPool)
import           Database.Selda.Backend (SeldaConnection, seldaClose)

import           Database.Selda.PostgreSQL      (withPostgreSQL, PG, PGConnectInfo, pgDatabase)

import Database.Selda (query, select, Row)
import           Database.PostgreSQL.Simple     (FromRow, ToRow)
import           GHC.Generics                   (Generic)
import           InterfaceAdapters.Config
import           Polysemy.Input
import           Polysemy.Internal.Union        (Member)
import           Polysemy.Trace                 (Trace, trace)
import           UseCases.KVS                   (KVS (..))

import InterfaceAdapters.Selda.Config (makePgConnPoolFromInfoSelda)
import InterfaceAdapters.Selda.ModelR (tableKV, KV, key, val)

data KeyValueRow = KeyValueRow T.Text T.Text
    deriving (Show, Generic, FromRow, ToRow)

-- instance FromRow KeyValueRow where
--   fromRow = KeyValueRow <$> field <*> field

--instance ToRow KeyValueRow where
--  toRow (KeyValueRow key_ val) = toRow (key_, val)

-- | Run a KVStore effect against a Selda backend. Requires a Config object as input.
runKvsAsSelda :: (Member (Embed IO) r, Member (Input PGConnectInfo) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v)
                   => Sem (KVS k v : r) a
                   -> Sem r a
runKvsAsSelda = interpret $ \case
  GetKvs k      -> getAction k
  ListAllKvs    -> listAction
  InsertKvs k v -> insertAction k v
  DeleteKvs k   -> deleteAction k

  where

    getAction :: (Member (Input PGConnectInfo) r, Member (Embed IO) r, Member Trace r, Show k, FromJSON v) => k -> Sem r (Maybe v)
    getAction key = undefined --do
      -- trace $ "getAction: " ++ show key
      -- conn <- connectionFrom input
      -- rows <- embed (SQL.queryNamed conn
      --                     "SELECT key, value FROM store WHERE key = :key"
      --                     [":key" := show key] :: IO [KeyValueRow])
      -- case rows of
      --   []                         -> return Nothing
      --   (KeyValueRow _key value):_ -> return $ (decode . encodeUtf8) value


    listAction :: (Member (Input PGConnectInfo) r, Member (Embed IO) r, Member Trace r, Read k, FromJSON v) => Sem r [(k, v)]
    listAction = do
      trace "listAction:"
      connInfo <- input
      rows <- embed (withPostgreSQL connInfo (query $ select tableKV) :: IO [KV])
      let maybeList = map toKeyValue rows
      return $ catNestedMaybe maybeList
        where
          toKeyValue kv =  ((read . T.unpack) (T.fromStrict $ key kv), (decode . encodeUtf8) (T.fromStrict $ val kv))

          catNestedMaybe [] = []
          catNestedMaybe ((key, Just value):xs) = (key, value):catNestedMaybe xs
          catNestedMaybe ((_  , Nothing):xs)    = catNestedMaybe xs


    insertAction :: (Member (Input PGConnectInfo) r, Member (Embed IO) r, Member Trace r, Show k, ToJSON v) => k -> v -> Sem r ()
    insertAction key value = undefined --do
      -- trace $ "insertAction: " ++ show key ++ " " ++ show (encode value)
      -- let (query, params) = ("INSERT INTO store (key, value) VALUES (:key, :value) "
      --                     <> "ON CONFLICT (key) DO UPDATE SET value = excluded.value",
      --                     [":key" := show key, ":value" := encodedValue])
      --                     where
      --                       encodedValue = (decodeUtf8 . encode) value
      -- conn <- connectionFrom input
      -- embed $ SQL.executeNamed conn query params


    deleteAction :: (Member (Input PGConnectInfo) r, Member (Embed IO) r, Member Trace r, Show k) => k -> Sem r ()
    deleteAction key = undefined --do
      -- trace $ "deleteAction: " ++ show key
      -- conn <- connectionFrom input
      -- embed $ SQL.executeNamed conn "DELETE FROM store WHERE key = :key" [":key" := show key]


    -- | create a connection based on configuration data, make sure table "store" exists.
    connectionFrom :: (Member (Embed IO) r, Member Trace r) => Sem r PGConnectInfo -> Sem r (Pool (SeldaConnection PG))
    connectionFrom pgConnectInfo = do
      connInfo <- pgConnectInfo
      trace $ "open connection to: " ++ unpack (pgDatabase connInfo)
      embed (makePgConnPoolFromInfoSelda connInfo)
        -- where
        --   getConnection :: FilePath -> IO SQL.Connection
        --   getConnection dbFile = do
        --     conn <- SQL.open dbFile
        --     SQL.execute_ conn "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
        --     return conn

{-# LANGUAGE TypeOperators #-}  -- to allow operators (like :*:) in types

module InterfaceAdapters.Selda.Write where

import Database.Selda --(SqlRow, Table, Text, Generic, table, primary, Attr((:-)), ID, autoPrimary)
import Database.Selda.PostgreSQL (withPostgreSQL, PG)
import qualified Data.HashMap as H
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)
-- import Data.Time.Format (formatTime, defaultTimeLocale)


import InterfaceAdapters.Selda.ModelR

import InterfaceAdapters.Selda.Config (pgConnectInfo)





dbModify :: SeldaT PG IO a -> IO a
dbModify command = do
  dbinfo <- pgConnectInfo
  withPostgreSQL dbinfo command


-- makeTablesIfTheyDontExist :: IO [()]
-- makeTablesIfTheyDontExist = mapM (dbModify . tryCreateTable) tablesAggregateMetricAttribute

makeTableIfNonExistent :: IO ()
makeTableIfNonExistent = dbModify $ tryCreateTable tablePerson


around n d =  (fromInteger $ round $ d * (10^n)) / (10.0^^n)
round2 d = around 2 d
mayberound n mf = fmap (around n) mf
mayberound2 mf = mayberound 2 mf


-- format = formatTime defaultTimeLocale "%F %T %Z"




savePerson :: [Person] -> SeldaM b Int
savePerson lP = do
  dt <- liftIO getCurrentTime
  insert tablePerson lP

{-# LANGUAGE FlexibleContexts #-}
{-
• Non type-variable argument in the constraint: SqlType (Maybe a)
  (Use FlexibleContexts to permit this)
• In the type signature:
    getAttribColDistinct :: SqlType (Maybe a) =>
                            Selector PubAttribute (Maybe a) -> SeldaM b [Maybe a]
-}
{-# LANGUAGE TypeFamilies #-}
{-
• Illegal equational constraint LeftCols (Col (Inner PG) af)
                                 ~ Col PG (Maybe af)
   (Use GADTs or TypeFamilies to permit this)
-}



module InterfaceAdapters.Selda.Read where


import Data.Text (Text)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Database.Selda -- (Selector, query, (:*:))
import Database.Selda.PostgreSQL (withPostgreSQL, PG, PGConnectInfo)

import InterfaceAdapters.Selda.ModelR (Person)
import InterfaceAdapters.Selda.Query
import InterfaceAdapters.Selda.Config (pgConnectInfo)

import InterfaceAdapters.Selda.Monad (DbM)


-- seldaRead :: PGConnectInfo -> SeldaT PG IO a -> IO a
-- seldaRead info cmd = do
--   dbinfo <- info
--   withPostgreSQL dbinfo cmd


dbreadR :: SeldaT PG IO a -> IO a
dbreadR command = do
  dbinfo <- pgConnectInfo
  withPostgreSQL dbinfo command

getPersons :: SeldaM b [Person]
getPersons = query queryPerson

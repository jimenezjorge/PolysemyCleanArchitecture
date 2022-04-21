{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}  -- to allow operators (like :*:) in types
{-# LANGUAGE FlexibleContexts #-}
{-
• Non type-variable argument
    in the constraint: Columns (LeftCols (Col (Inner s) a))
  (Use FlexibleContexts to permit this)
-}
{-# LANGUAGE TypeFamilies #-}
{-
• Illegal equational constraint LeftCols
                                  (Col (Inner (Inner s)) af)
                                ~ Col (Inner s) (Maybe af)
  (Use GADTs or TypeFamilies to permit this)
-}


{-|
Module      : Db.Selda.Query
Description : Selda Queries

This module contains all the Selda Queries.

All queries are in the monad Query s a.
Immediately you ask: I thought monads had only 1 type argument, how come Query has two?
Well, the type parameter s is actually a phantom type, i.e. it does not occur in the data constructor of Query.
(It is used for keeping track of the scope when the query has nested queries.)

Since we have mentioned Query s a here,
all the subsequent documentation for the functions in this module
will mention only the type parameter a.
-}
module InterfaceAdapters.Selda.Query where

import Database.Selda
import Database.Selda.Backend (Backend)

import InterfaceAdapters.Selda.ModelR



-- | Just the general query runner restated here to be included when importing this module into GHCi.
runQuery :: (Result a, MonadSelda m) => Query (Backend m) a -> m [Res a]
runQuery = query

queryPerson :: Query s (Row s Person)
queryPerson = do
  person <- select tablePerson
  return person

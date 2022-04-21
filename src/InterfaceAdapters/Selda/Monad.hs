{-# LANGUAGE TypeSynonymInstances #-}  -- to allow instance MonadSelda DbM
{-     • Illegal instance declaration for ‘MonadSelda DbM’
        (All instance types must be of the form (T t1 ... tn)
         where T is not a synonym.
         Use TypeSynonymInstances if you want to disable this.)
-}
{-# LANGUAGE FlexibleInstances #-}
{-    • Illegal instance declaration for ‘MonadSelda DbM’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
-}
{-# LANGUAGE TypeFamilies #-}
{-    • Illegal family instance for ‘Backend’
        (Use TypeFamilies to allow indexed type families)
-}

{-

-}
module InterfaceAdapters.Selda.Monad where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (asks)
import Data.Pool (withResource)

import Database.Selda (MonadSelda)
import Database.Selda.Backend (Backend, withConnection)
import Database.Selda.PostgreSQL (PG)

import InterfaceAdapters.Selda.Config (DbConnPool, getDbConnPool)



type DbM = ReaderT DbConnPool IO

instance MonadSelda DbM where
  type Backend DbM = PG      -- this syntax needs TypeFamilies, but GHC won't suggest it
  withConnection action = do
    pool <- asks getDbConnPool
    withResource pool action

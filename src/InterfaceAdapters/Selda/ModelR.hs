{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Db.Selda.ModelR
Description : Selda Models

In Selda a record is used as a model representing a database table.
The names of the record fields must be the same as the names of the columns in the database table.
Because database table columns usually follow the snake-case convention,
we have underscores in the field names of our records, which seems a bit unHaskelly.
However this has the beneficial side-effect that if we see a snake-cased identifier, we know
It also has the side-effect that when we name a Haskell variable representing the column with the usual camelcase,
it allowing us to distinguish them on sight.

Nullable column vs column with nulls

Any single-constructor type can be used to represent a database table.

-}

module InterfaceAdapters.Selda.ModelR where

import Data.Time (LocalTime)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Database.Selda (SqlRow, Table, Text, Generic, table, primary, Attr((:-)), Selector)


data KV = KV
  { key :: Text
  , val :: Text
  } deriving (Eq, Generic, Show)
instance SqlRow KV

tableKV :: Table KV
tableKV = table "kv" [#key :- primary]

data Person = Person
  { iD :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , uuid :: Text
  , age :: Int
  , firstName :: Text
  , lastName :: Text
  , accountConfirmed :: Bool
  , deactivated :: Bool
  , purged :: Bool
  , droppedDate :: UTCTime
  , purgedDate :: UTCTime
  } deriving (Eq, Generic, Show)
instance SqlRow Person

type PersonCol = Selector Person (Maybe Double)

tablePerson :: Table Person
tablePerson = table "person" [#iD :- primary]


-- type AttrColBool = Selector PubAttribute (Maybe Int)
-- type AttrColEnum = Selector PubAttribute (Maybe Int)

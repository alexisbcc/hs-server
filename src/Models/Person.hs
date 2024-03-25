{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Person
  ( Person,
    PersonBaseData,
    getPeople,
    getPerson,
    createPerson,
    updatePerson,
    deletePerson,
  )
where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Int (Int64)
import Data.Time (UTCTime (..))
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Only (Only),
    execute,
    query,
    query_,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Servant (NoContent (NoContent))

data PersonBaseData = PersonBaseData
  { pbd_first_name :: String,
    pbd_last_name :: String,
    pbd_email :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''PersonBaseData)

data Person = Person
  { uid :: Int64,
    first_name :: String,
    last_name :: String,
    email :: String,
    registration_date :: UTCTime
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Person)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field <*> field

getPeople :: Connection -> IO [Person]
getPeople conn = query_ conn "SELECT * FROM person"

getPerson :: Connection -> Int -> IO [Person]
getPerson conn personId = query conn "SELECT * FROM person WHERE uid = ?" (Only personId)

createPerson :: Connection -> PersonBaseData -> IO [Only Int64]
createPerson conn person = query conn "INSERT INTO person VALUES (DEFAULT,?,?,?,current_timestamp) RETURNING uid" [pbd_first_name person, pbd_last_name person, pbd_email person]

updatePerson :: Connection -> Int -> PersonBaseData -> IO Bool
updatePerson conn personId person = do
  n <- execute conn "UPDATE person SET first_name = ?, last_name = ?, email = ? WHERE uid = ?" (pbd_first_name person, pbd_last_name person, pbd_email person, personId)
  return $ n > 0

deletePerson :: Connection -> Int -> IO Bool
deletePerson conn personId = do
  n <- execute conn "DELETE FROM person WHERE uid = ?" (Only personId)
  return $ n > 0

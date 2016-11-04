{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module MySafeStore
  (
    initDB
  , insertDB
  , preparedInsertDB
  , readDB
  , closeDB
  , sanitize

  , DataStore
  , UserInput
  , readInput

  ) where

import Prelude hiding (takeWhile)
import Database.SQLite3
import Data.Monoid ((<>))
import Data.Text (Text, takeWhile)


newtype DataStore = DataStore {
  conn :: Database
}

data Clean
data Dirty

newtype UserInput a = UserInput {
  input :: Text
}

readInput :: Text -> UserInput Dirty
readInput = UserInput

initDB :: IO DataStore
initDB = do
  conn <- open ":memory:"
  exec conn "CREATE TABLE IF NOT EXISTS person (id INTEGER PRIMARY KEY, name TEXT)"
  exec conn "INSERT INTO person (name) VALUES ('Tim')"
  exec conn "INSERT INTO person (name) VALUES ('Mark')"
  exec conn "INSERT INTO person (name) VALUES ('Sarah')"
  return $ DataStore conn

insertDB :: DataStore -> UserInput Clean -> IO ()
insertDB DataStore{..} UserInput{..} =
  exec conn $ "INSERT INTO person (name) VALUES ('" <> input <> "')"

-- The way you should *actually* build up this insert.  :)
preparedInsertDB :: DataStore -> Text -> IO ()
preparedInsertDB DataStore{..} name = do
  statement <- prepare conn "INSERT INTO person (name) VALUES (?)"
  bindSQLData statement 1 (SQLText name)
  _ <- step statement
  return ()

readDB :: DataStore -> IO ()
readDB DataStore{..} = execPrint conn "select * from person"

closeDB :: DataStore -> IO ()
closeDB =  close . conn

-- Please don't use this sanitization function in real life!
sanitize :: UserInput Dirty -> UserInput Clean
sanitize =
  UserInput . takeWhile (`notElem` [';','\'','"']) . input

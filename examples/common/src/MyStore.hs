{-# LANGUAGE OverloadedStrings #-}

module MyStore where

import Prelude hiding (takeWhile)
import Database.SQLite3
import Data.Monoid ((<>))
import Data.Text (Text, takeWhile)


initDB :: IO Database
initDB = do
  conn <- open ":memory:"
  exec conn "CREATE TABLE IF NOT EXISTS person (id INTEGER PRIMARY KEY, name TEXT)"
  exec conn "INSERT INTO person (name) VALUES ('Tim')"
  exec conn "INSERT INTO person (name) VALUES ('Mark')"
  exec conn "INSERT INTO person (name) VALUES ('Sarah')"
  return conn

insertDB :: Database -> Text -> IO ()
insertDB conn name =
  exec conn $ "INSERT INTO person (name) VALUES ('" <> name <> "')"

readDB :: Database -> IO ()
readDB = flip execPrint "select * from person"

closeDB :: Database -> IO ()
closeDB =  close

-- Please don't use this sanitization function in real life!
sanitize :: Text -> Text
sanitize =
  takeWhile (`notElem` [';','\'','"'])

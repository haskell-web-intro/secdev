{-# LANGUAGE OverloadedStrings #-}
module PhantomTypes where

import MyStore
--import MySafeStore


main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  conn <- initDB
  print "Contents of initial database:"
  readDB conn

  insertDB conn "Haskell Curry"
  print "Contents after insert of our saint"
  readDB conn

  insertDB conn (sanitize "Evil'); drop table person; --")
  print "Contents after allowing my sadistic co-worker to use my library"
  readDB conn

  insertDB conn "Evil'); drop table person; --"
  print "Contents after allowing my sadistic co-worker to use my library a second time"
  readDB conn


{-
main2 :: IO ()
main2 = do
  conn <- initDB
  print "Contents of initial database:"
  readDB conn

  insertDB conn (sanitize (readInput "Haskell Curry"))
  print "Contents after insert of our saint"
  readDB conn

  insertDB conn (readInput $ "Evil'); drop table person; --")
  print "Contents after allowing my sadistic co-worker to use my library"
  readDB conn
-}

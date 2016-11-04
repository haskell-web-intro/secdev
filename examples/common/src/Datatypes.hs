{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Datatypes where
import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data HelloWorld = HelloWorld

instance ToJSON HelloWorld where
  toJSON = const $ object [ "hello" .= ("world" :: Text) ]

data Car = Car {
  make  :: String
, model :: String
, year  :: Int
} deriving (Generic)
instance ToJSON Car

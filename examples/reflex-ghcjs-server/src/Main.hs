{-# LANGUAGE TypeOperators, DataKinds #-}
module Main where

import Data.Text (Text)

import Network.Wai.Handler.Warp
import Servant
import Text.HTML.SanitizeXSS (sanitize)




type SanitizeApi =
       "sanitize" :> ReqBody '[PlainText] Text :> Post '[PlainText] Text
  :<|> Raw

sanitizer :: Text -> Handler Text
sanitizer = return . sanitize

endpoints :: Server SanitizeApi
endpoints = sanitizer :<|> serveDirectory "static"

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy SanitizeApi) endpoints

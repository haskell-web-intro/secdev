{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, DeriveGeneric, MultiParamTypeClasses #-}
module LogApi where

import Data.Aeson
import Data.Swagger (Swagger, ToSchema)
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)

import GHC.Generics
import Servant
import Servant.Client (client)
import Servant.Swagger (toSwagger)



data Status = OK | Fail
  deriving (Generic, Show, Read, Ord, Eq)

instance ToSchema Status

instance MimeRender PlainText Status where
  mimeRender _ OK   = "OK"
  mimeRender _ Fail = "Fail"

instance MimeUnrender PlainText Status where
  mimeUnrender _ "OK"   = Right OK
  mimeUnrender _ "Fail" = Right Fail
  mimeUnrender _ bs     = Left ("Invalid status: " ++ (unpack . decodeUtf8) bs)

data Log = Log {
  ident :: Int
, timestamp :: Int
, message :: Text
} deriving (Generic, Show, Typeable)

instance ToJSON Log
instance FromJSON Log
instance ToSchema Log

-- API Specification
type LogApi = "logs" :> Get '[JSON] [Log]
         :<|> "log"  :> ReqBody '[JSON] Log :> Post '[PlainText] Status
         :<|> "log"  :> Capture "id" Int :> Get '[JSON] Log

logApi :: Proxy LogApi
logApi = Proxy

-- Handlers for the LogApi
foo :: Log -> Handler Status
foo _ = return OK

bar :: Handler [Log]
bar = return [
    Log {ident = 1, timestamp = 101, message = "msg1"}
  , Log {ident = 2, timestamp = 201, message = "msg2"}
  ]

baz :: Int -> Handler Log
baz n = return Log {ident = n, timestamp = 102, message = "message"}

-- Server implementation -- to be filled in
server :: Server LogApi
server = undefined :<|> undefined :<|> undefined


-- Generate clients for our api
(getLogs :<|> postLog :<|> getLogById) = client logApi

-- Generate Swagger specification of api
swaggerDocs :: Swagger
swaggerDocs = toSwagger logApi

main :: IO ()
main =
  run 8080 . logStdout $ serve logApi server

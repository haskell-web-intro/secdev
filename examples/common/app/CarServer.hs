{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings #-}
module CarServer where

import Servant
import Control.Monad.Trans.Except
import Network.Wai.Handler.Warp

import Datatypes

type API = "sayhello" :> Get '[JSON] HelloWorld
      :<|> "tesla"    :> Capture "year" Int  :> Get '[JSON] Car
      :<|> Raw

helloHandler :: ExceptT ServantErr IO HelloWorld
helloHandler = return HelloWorld

carHandler :: Int -> ExceptT ServantErr IO Car
carHandler inputyear
  | inputyear < 2012 =  throwError $ err501 { errBody = "too old!"}
  | otherwise        =  return $ Car "Tesla" "Model S" inputyear

endpoints :: Server API
endpoints = helloHandler
       :<|> carHandler
       :<|> serveDirectory "static"

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy API) endpoints

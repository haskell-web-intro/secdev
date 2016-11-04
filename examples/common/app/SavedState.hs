{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings, DeriveGeneric #-}
module SavedState where

import           Servant
import           Control.Monad.Trans.Except
import           Network.Wai.Handler.Warp
import           GHC.Generics
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Monoid
import           Data.Aeson
import           Control.Concurrent.STM
import           Control.Monad.Morph
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask)

data Yuge = Yuge {
  howyuge :: Int
, pronunciation :: Text
} deriving (Generic)
instance ToJSON Yuge

type MyStore = TVar Yuge

type API = "yuge" :> Get '[JSON] Yuge
      :<|> "yuge" :> Capture "u" Int  :> Get '[JSON] Text

makeYuge :: Int -> Yuge
makeYuge numUs = Yuge numUs ("y" <> uuu <> "ge")
  where uuu = T.pack $ replicate numUs 'u'

getYuge :: ReaderT MyStore (ExceptT ServantErr IO) Yuge
getYuge = do
  currentYuge <- ask
  liftIO (readTVarIO currentYuge)

tryIncreasingYuge :: Int -> MyStore -> ExceptT ServantErr STM Yuge
tryIncreasingYuge requestedUs yugeStore = do
  currentYuge <- lift $ readTVar yugeStore
  if requestedUs <= howyuge currentYuge
    then throwError $ err501 { errBody = "not " <> toBytes (pronunciation currentYuge) <>  " enough!"}
    else do
      let newyuge = makeYuge requestedUs
      lift $ writeTVar yugeStore newyuge
      return newyuge
  where toBytes = LT.encodeUtf8 . LT.fromStrict

setYuge :: Int -> ReaderT MyStore (ExceptT ServantErr IO) Text
setYuge requestedUs = do
  yugeStore <- ask
  pronunciation <$> lift (hoist atomically (tryIncreasingYuge requestedUs yugeStore))

endpoints :: MyStore -> Server API
endpoints mystore = enter addStore getYuge
               :<|> enter addStore setYuge
  where addStore = runReaderTNat mystore

main :: IO ()
main = do
  initialYuge <- atomically ( newTVar ( Yuge 1 "yuge"))
  run 8080 $ serve (Proxy :: Proxy API) (endpoints initialYuge)

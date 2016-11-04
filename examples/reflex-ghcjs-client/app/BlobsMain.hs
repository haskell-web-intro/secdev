{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
import           Prelude hiding (div)
import           Reflex
import           Reflex.Dom hiding (fromJSString)

import           Control.Monad.IO.Class (liftIO)

import           Blobs

import           MdlWidgets

main :: IO ()
main = mainWidget $ do
  gaussians <- grid $ do
    g1 <- cell6 parameters
    g2 <- cell6 parameters
    return $ (\x y -> quantizeGaussian [x,y]) <$> g1 <*> g2
  initG <- sample $ current gaussians
  grid $ do
    cell6 ( elAttr "div" ("id" =: "topdown") blank )
    cell6 ( elAttr "div" ("id" =: "threed") blank )
  performEvent_ (liftIO . plotAction <$> updated gaussians)

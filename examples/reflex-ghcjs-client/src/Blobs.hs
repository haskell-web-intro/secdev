{-# LANGUAGE RecursiveDo, RecordWildCards, OverloadedStrings, ScopedTypeVariables  #-}
module Blobs where

import           Prelude hiding (div)
import           Reflex
import           Reflex.Dom hiding (fromJSString)
import           Data.Monoid
import           Data.Text (Text)
import           Data.JSString (pack, JSString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Aeson
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           MdlWidgets
import qualified MdlWidgets as CSS

data GaussianParameters a = GaussianParameters {
  sigmaX :: a
, sigmaY :: a
, x0     :: a
, y0     :: a
, alpha  :: a }

-- https://en.wikipedia.org/wiki/Gaussian_function#Two-dimensional_Gaussian_function
gaussian :: (Fractional a, Floating a) => GaussianParameters a -> a -> a -> a
gaussian GaussianParameters{..} x y =
  alpha * exp ( negate ( xnumerator / xdenominator + ynumerator / ydenominator) )
  where
    xnumerator   = (x - x0) ** 2
    xdenominator = 2 * (sigmaX ** 2)
    ynumerator   = (y - y0) ** 2
    ydenominator = 2 * (sigmaY ** 2)

quantizeGaussian :: [GaussianParameters Float] -> Vector (Vector Float)
quantizeGaussian blobs = V.map (\y -> V.map (\x -> sum $ (\param -> gaussian param x y) <$> blobs) minus25to25) minus25to25

minus25to25 :: Vector Float
minus25to25 = V.enumFromN (-25) 50

rangeEv :: MonadWidget t m => Float -> Float -> Float -> m (Event t Float)
rangeEv minval maxval initval = do
  r <- rangeInput cfg
  materialInitialize (_rangeInput_element r)
  return $ _rangeInput_input r
  where
    attrs = CSS.sliderClass <>
            ("type" =: "range") <>
            ("min" =: tshow minval) <>
            ("max" =: tshow maxval)
    cfg = def
            & rangeInputConfig_initialValue .~ initval
            & rangeInputConfig_attributes .~ constDyn attrs


labeledRange :: MonadWidget t m => Text -> Float -> Float -> Float -> m (Event t Float)
labeledRange label minval maxval initval = grid $ mdo
  cell4 $ mdlChip label (fshow <$> rdy)
  rev <- cell8 $ rangeEv minval maxval initval
  rdy <- holdDyn initval rev
  return rev
  where fshow f = let i :: Int = round f in tshow i

data Parameters = SigmaX Float | SigmaY Float | X0 Float | Y0 Float deriving (Ord, Eq)

parameters :: MonadWidget t m  => m (Dynamic t (GaussianParameters Float))
parameters = mdlCard $ do
  sxev <- labeledRange "sx" 0 10 5
  syev <- labeledRange "sy" 0 10 5
  xev <-  labeledRange "x" (-25) 25 0
  yev <-  labeledRange "y" (-25) 25 0
  let initialblob :: GaussianParameters Float = GaussianParameters 5 7 0 0 1
  foldDyn updateBlob initialblob (leftmost [SigmaX <$> sxev, SigmaY <$> syev, X0 <$> xev, Y0 <$> yev])

updateBlob :: Parameters -> GaussianParameters Float -> GaussianParameters Float
updateBlob (SigmaX sx) g = g { sigmaX = sx }
updateBlob (SigmaY sy) g = g { sigmaY = sy }
updateBlob (X0 x0)     g = g { x0     = x0 }
updateBlob (Y0 y0)     g = g { y0     = y0 }

plotBlob :: JSString -> Text -> Vector (Vector Float) -> IO ()
plotBlob elementId plotType plotData = plot elementId (pack jsondata)
  where
    jsondata = pak $ object ["type" .= plotType
                            , "z" .= plotData
                            , "x" .= minus25to25
                            , "y" .= minus25to25
                            , "colorscale" .= ("Viridis" :: Text)]
    pak = LBSC.unpack . encode . V.singleton

plotAction :: Vector (Vector Float) -> IO ()
plotAction plotData = do
  plotBlob "topdown" "heatmap" plotData
  plotBlob "threed"  "surface" plotData

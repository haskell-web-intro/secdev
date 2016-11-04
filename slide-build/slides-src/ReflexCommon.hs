{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module ReflexCommon where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Event)

import Reflex.Dom

runReflexWidget :: String ->
                   (forall x. Widget x ()) ->
                   IO ()
runReflexWidget elemId w = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap castToHTMLDocument <$> webViewGetDomDocument webView
  Just e   <- fmap castToHTMLElement  <$> getElementById doc elemId
  attachWidget e webViewSing w

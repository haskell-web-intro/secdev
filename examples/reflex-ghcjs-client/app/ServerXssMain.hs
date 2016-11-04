{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import Reflex

import MdlWidgets
import ServerSafeDom

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.Types hiding (Event)

widget :: MonadWidget t m => m ()
widget =  grid $ do
  cell2 blank
  cell8 $ do

    inp <- mdlInput (getInput defaultInput)

    let inpEv   = _textInput_input inp
    sanitizedEv <- sanitize inpEv

    safeDyn <- foldDyn const defaultInput sanitizedEv

    grid $ do
      cell3 blank
      cell6 $ mdlTitleCard "Sanitized" "sani" $ elDynHtml "span" safeDyn
      cell3 blank
  cell2 blank

main :: IO ()
main = runReflexWidget "xss-code" widget

runReflexWidget :: String ->
                   (forall x. Widget x ()) ->
                   IO ()
runReflexWidget elemId w = runWebGUI $ \webView -> withWebViewSingleton webView $ \webViewSing -> do
  Just doc <- fmap castToHTMLDocument <$> webViewGetDomDocument webView
  Just e   <- fmap castToHTMLElement  <$> getElementById doc elemId
  attachWidget e webViewSing w

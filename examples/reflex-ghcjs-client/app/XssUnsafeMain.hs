{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Data.Monoid ((<>))
import Data.Text (Text)

import Reflex
import Reflex.Dom

import MdlWidgets

import Text.HTML.SanitizeXSS

main :: IO ()
main = mainWidget $ do
  inputStream

inputStream :: MonadWidget t m => m ()
inputStream = do
  grid $ do
    cell2 blank
    cell8 $ do
      let defInp  = "<span>spooky user input</span>"

      inp <- mdlInput defInp
      let inpEv   = _textInput_input inp

      safeDyn   <- foldDyn (\n _ -> sanitize n) defInp inpEv
      unsafeDyn <- foldDyn const defInp inpEv

      grid $ do
        cell6 $ mdlTitleCard "Sanitized"    "sani"   $ elDynHtml' "span" safeDyn
        cell6 $ mdlTitleCard "Un-Sanitized" "unsani" $ elDynHtml' "span" unsafeDyn
      blank
    cell2 blank

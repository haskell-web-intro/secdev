{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Reflex

import MdlWidgets
import ClientSafeDom

main :: IO ()
main = mainWidget inputStream

inputStream :: MonadWidget t m => m ()
inputStream =
  grid $ do
    cell2 blank
    cell8 $ do
      let defInpT = "<span>spooky user input</span>"
          defInp  = sanitize $ inject defInpT

      inp <- mdlInput defInpT
      let inpEv   = _textInput_input inp

      safeDyn <- foldDyn (\n _ -> sanitize n) defInp inpEv

      grid $ do
        cell3 blank
        cell6 $ mdlTitleCard "Sanitized" "sani" $ elDynHtml "span" safeDyn
        cell3 blank

      blank
    cell2 blank

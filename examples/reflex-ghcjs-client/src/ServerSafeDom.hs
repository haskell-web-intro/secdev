{-# LANGUAGE OverloadedStrings #-}

module ServerSafeDom
  ( UserInput
  , Clean
  , Dirty
  , sanitize
  , readInput
  , getInput

  -- Functions re-exported from Reflex.Dom
  , module RDExport

  -- Safe reimplementation of functions from ReflexDom
  , elDynHtml
  , _textInput_value
  , _textInput_input

  -- Static strings we know are good
  , defaultInput

  ) where

import Reflex.Dom as RDExport hiding (elDynHtml', _textInput_value, _textInput_input)
import qualified Reflex.Dom as RD

import Data.Text (Text)


-- Wrapped Dom elements
elDynHtml :: MonadWidget t m => Text -> Dynamic t (UserInput Clean) -> m (El t)
elDynHtml e = RD.elDynHtml' e . fmap getInput

_textInput_value :: Reflex t => TextInput t -> Dynamic t (UserInput Dirty)
_textInput_value = fmap UserInput . RD._textInput_value

_textInput_input :: Reflex t => TextInput t -> Event t (UserInput Dirty)
_textInput_input = fmap UserInput . RD._textInput_input

data Clean
data Dirty

newtype UserInput a = UserInput {
  getInput :: Text
}

readInput :: Text -> UserInput Dirty
readInput = UserInput

sanitize :: MonadWidget t m => Event t (UserInput Dirty) -> m (Event t (UserInput Clean))
sanitize inp = do
  evResp <- performRequestAsync $ fmap makeReq inp
  let evInp = _xhrResponse_responseText <$> evResp
  return $ fmap (maybe (UserInput "") UserInput) evInp

makeReq :: UserInput Dirty -> XhrRequest Text
makeReq inp =
  xhrRequest
    "POST" "/sanitize"
    (def {_xhrRequestConfig_sendData = getInput inp} )

defaultInput :: UserInput Clean
defaultInput = UserInput "<span>spooky user input</span>"

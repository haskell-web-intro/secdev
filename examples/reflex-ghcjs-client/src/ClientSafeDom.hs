{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module ClientSafeDom
  ( UserInput
  , Clean
  , Dirty
  , sanitize
  , inject
  , getInput

  -- Functions re-exported from Reflex.Dom
  , module RDExport

  -- Safe reimplementation of functions from ReflexDom
  , elDynHtml
  , _textInput_value
  , _textInput_input

  ) where

import Reflex.Dom as RDExport hiding (elDynHtml', _textInput_value, _textInput_input)
import qualified Reflex.Dom as RD

import Data.Text (Text)
import qualified Text.HTML.SanitizeXSS as S



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

inject :: Text -> UserInput Dirty
inject = UserInput

sanitize :: UserInput Dirty -> UserInput Clean
sanitize UserInput{..} = UserInput (S.sanitize getInput)

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module MdlWidgets where

import           Control.Lens
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)
import           Data.Map (Map)

import           Reflex.Dom
import           GHCJS.Types
import qualified GHCJS.DOM.Types as JS
import           GHCJS.DOM.Element (toElement)


foreign import javascript unsafe "Plotly.newPlot($1, JSON.parse($2));" plot :: JSString -> JSString -> IO ()
foreign import javascript unsafe "componentHandler.upgradeElement($1);" materialInitJS :: JSVal -> IO () -- http://www.getmdl.io/started/index.html#dynamic

tshow :: Show a => a -> Text
tshow = T.pack . show

materialInitialize :: (MonadWidget t m, JS.IsHTMLElement e) => e -> m ()
materialInitialize e = do
  let jsel = JS.unElement . toElement $ e
  pb <- getPostBuild
  performEvent_ $ liftIO (materialInitJS jsel) <$ pb

grid :: forall t m a. MonadWidget t m => m a -> m a
grid = divClass "mdl-grid"

--btn :: MonadWidget t m => Text -> m ()
btn :: forall t m. MonadWidget t m => Text -> m (El t)
btn txt = do
  (e,_) <- elAttr'
    "button"
    ("class" =: "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect")
    (text txt)
  materialInitialize (_element_raw e)
  return e

mkcell :: forall t m a. MonadWidget t m => Int -> m a -> m a
mkcell c = divClass $ "mdl-cell mdl-cell--" <> tshow c <> "-col"

cell1, cell2, cell3, cell4, cell5, cell6, cell8 :: forall t m a. MonadWidget t m => m a -> m a
cell1 = mkcell 1
cell2 = mkcell 2
cell3 = mkcell 3
cell4 = mkcell 4
cell5 = mkcell 5
cell6 = mkcell 6
cell8 = mkcell 8

mdlInput :: forall t m. MonadWidget t m => Text -> m (TextInput t)
mdlInput =
  flip mdlInput' never

mdlInput' :: forall t m . MonadWidget t m => Text -> Event t Text -> m (TextInput t)
mdlInput' initVal ev =
  divClass "mdl-textfield mdl-js-textfield whole-block" $ do
    t <- textInput (def
                      & textInputConfig_initialValue .~ initVal
                      & textInputConfig_setValue .~ ev
                      & textInputConfig_attributes .~ constDyn ("class" =: "mdl-textfield__input")
                   )
    materialInitialize (_textInput_element t)
    return t

mdlChip :: MonadWidget t m => Text -> Dynamic t Text -> m ()
mdlChip badgeLabel chipText = do
  elAttr' "span" ("class" =: "mdl-chip mdl-chip--contact") $ do
    elAttr' "span" ("class" =: "mdl-chip__contact mdl-color--pink mdl-color-text--white") $ text badgeLabel
    elAttr' "span" ("class" =: "mdl-chip__text") $ dynText chipText
  blank

mdlCard :: forall t m a. MonadWidget t m => m a -> m a
mdlCard = elAttr "div" ("class" =: "mdl-card mdl-shadow--4dp" <> "style" =: "width: 100%;")

mdlTitleCard :: forall t m a. MonadWidget t m => Text -> Text -> m a -> m a
mdlTitleCard title cardId inner = do
  elAttr "div" ("class" =: "mdl-card mdl-shadow--4dp" <> "style" =: "width: 100%;" <> "id" =: cardId) $ do
    divClass "mdl-card__title" $
      elAttr "h2" ("class" =: "mdl-card__title-text") $ text title
    inner

sliderClass :: Map Text Text
sliderClass = "class" =: "mdl-slider mdl-js-slider"

header :: MonadWidget t m => m ()
header = do
  -- elAttr' ("src" =: "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js") blank
  elAttr' "script" ("src" =: "https://cdn.plot.ly/plotly-latest.min.js") blank
  elAttr' "script" (("defer" =: "") <> "src" =: ("https://code.getmdl.io/1.2.0/material.min.js")) blank
  elAttr' "link" (("rel" =: "stylesheet") <> "src" =: ("https://fonts.googleapis.com/icon?family=Material+Icons")) blank
  elAttr' "link" (("rel" =: "stylesheet") <> "src" =: ("https://code.getmdl.io/1.2.0/material.indigo-pink.min.css")) blank
  blank

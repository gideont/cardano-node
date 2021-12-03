module Cardano.Tracer.Handlers.RTView.UI.Utils
  ( (##)
  , dataTooltip
  , findAndDo
  , image
  , showIt
  , showInline
  , hideIt
  , pageTitle
  , pageTitleNotify
  ) where

import           Data.Text (Text, unpack)
import           Control.Monad.Extra (whenJustM)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

image
  :: String
  -> String
  -> UI Element
image imgClass svg = UI.span #. imgClass # set html svg

(##) :: UI Element -> String -> UI Element
(##) el anId = el # set UI.id_ anId

findAndDo
  :: UI.Window
  -> Text
  -> (Element -> UI ())
  -> UI ()
findAndDo window anId action =
  whenJustM (UI.getElementById window (unpack anId)) action

showIt, showInline, hideIt :: UI Element -> UI Element
showIt     = set style [("display", "block")]
showInline = set style [("display", "inline")]
hideIt     = set style [("display", "none")]

pageTitle, pageTitleNotify :: String
pageTitle       = "Cardano RTView"
pageTitleNotify = "(!) Cardano RTView"

dataTooltip :: WriteAttr Element String
dataTooltip = mkWriteAttr $ set' (attr "data-tooltip")

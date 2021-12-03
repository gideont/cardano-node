{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo
  ( mkOwnInfo
  ) where

import           Control.Monad (forM, void)
import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, text, (#), (#+), (#.))
import           System.FilePath.Posix (takeDirectory)

import           Cardano.Config.Git.Rev (gitRev)

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Paths_cardano_tracer (version)

{-
import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (configFileIsProvided, notificationsFileIsProvided,
                                        logFilesDir, savedConfigurationFile,
                                        savedNotificationsFile, savedRTViewParamsFile)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), (#.), hideIt)
import           Cardano.RTView.GUI.JS.Utils (copyTextToClipboard)
import           Cardano.RTView.Git.Rev (gitRev)
import           Paths_cardano_rt_view (version)
-}

mkOwnInfo :: Element -> UI Element
mkOwnInfo closeIt =
  UI.div #. "modal" #+
    [ UI.div #. "modal-background" #+ []
    , UI.div #. "modal-content" #+
        [ UI.div #. "container" #+
            [ UI.div #. "box" #+
                [ UI.div #. "columns" #+
                    [ UI.div #. "column has-text-right" #+
                        [ UI.p #. "mb-1" #+
                            [ string "Version"
                            , image "rt-view-what-icon" questionSVG
                                    -- # set (dataAttr "tooltip") "Version of cardano-tracer RTView is a part of"
                            ]
                        , UI.p #. "mb-1" #+
                            [ string "Commit"
                            , image "rt-view-what-icon" questionSVG
                                    # set UI.title__ "Git commit cardano-tracer was built from"
                            ]
                        , UI.p #. "mb-1" #+
                            [ string "Platform"
                            , image "rt-view-what-icon" questionSVG
                                    # set UI.title__ "Platform cardano-tracer is running on"
                            ]
                        ]
                    , UI.div #. "column has-text-weight-semibold" #+
                        [ UI.p #. "mb-1" #+ [string $ showVersion version]
                        , UI.p #. "mb-1" #+ [string . T.unpack . T.take 7 $ gitRev]
                        , UI.p #. "mb-1" #+ [string platform]
                        ]
                    ]
                ]
            ]
        ]
    , element closeIt
    ]

platform :: String
#if defined(mingw32_HOST_OS)
platform = "Windows"
#elif defined(darwin_HOST_OS)
platform = "Mac"
#else
platform = "Linux"
#endif

-- <button class="button is-primary" data-tooltip="Tooltip Text">Tooltip</button>

{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Plugin where

import Clckwrks
import Clckwrks.Page.PreProcess    (pageCmd)
import Clckwrks.Route              (routeClck)
import Data.Text                   (Text)
import qualified Data.Text.Lazy as TL
import Web.Plugins.Core            (Plugin(..), addHandler, getPluginRouteFn, initPlugin)

clckHandler :: (ClckURL -> [(Text, Maybe Text)] -> Text)
            -> ClckPlugins
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
clckHandler showRouteFn _plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) -> routeClck u

clckInit :: ClckPlugins
         -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       addPreProc plugins (pageCmd clckShowFn)
       addHandler plugins (pluginName clckPlugin) (clckHandler clckShowFn)
       return Nothing


clckPlugin :: Plugin ClckURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ([TL.Text -> ClckT ClckURL IO TL.Text])
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return ()
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI clckPlugin

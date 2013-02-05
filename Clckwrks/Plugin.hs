{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Plugin where

import Clckwrks
import Clckwrks.Menu.Types (MenuLink(..))
import Clckwrks.Route              (routeClck)
import Control.Monad.State         (get)
import Data.Text                   (Text)
import qualified Data.Set          as Set
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

clckMenuCallback :: ClckT ClckURL IO (String, [MenuLink])
clckMenuCallback =
    do adminURL <- showURL (Admin Console)
       return ("Clck", [MenuLink "Admin" adminURL])

clckInit :: ClckPlugins
         -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       addMenuCallback plugins clckMenuCallback
       addHandler plugins (pluginName clckPlugin) (clckHandler clckShowFn)
       return Nothing

addClckAdminMenu :: ClckT url IO ()
addClckAdminMenu =
    do p <- plugins <$> get
       (Just clckShowURL) <- getPluginRouteFn p (pluginName clckPlugin)
       addAdminMenu ( "Profile"
                    , [ (Set.fromList [Administrator, Visitor], "Edit Your Profile"      , clckShowURL (Profile EditProfileData) [])
                      ]
                    )

       addAdminMenu ( "Clckwrks"
                    , [ (Set.singleton Administrator, "Console"      , clckShowURL (Admin Console)      [])
                      , (Set.singleton Administrator, "Edit Settings", clckShowURL (Admin EditSettings) [])
                      , (Set.singleton Administrator, "Edit Menu"    , clckShowURL (Admin EditMenu)     [])
                      ]
                    )

clckPlugin :: Plugin ClckURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = addClckAdminMenu
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI clckPlugin

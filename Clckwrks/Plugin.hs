{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Plugin where

import Clckwrks
import Clckwrks.Rebac.Acid         (clckwrksFragment, clckwrksRels)
import Clckwrks.Rebac.Administrators (AdministratorsChange(..), syncAdministrators)
import Clckwrks.Rebac.URL          (RebacURL(..))
import Clckwrks.Types              (NamedLink(..))
import Clckwrks.Route              (routeClck)
import Control.Monad.State         (get)
import Data.Text                   (Text)
import qualified Data.Set          as Set
import qualified Data.Text.Lazy as TL
import Web.Plugins.Core            (Plugin(..), addHandler, getPluginRouteFn, getPluginsSt, initPlugin)

clckHandler :: (ClckURL -> [(Text, Maybe Text)] -> Text)
            -> ClckPlugins
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
clckHandler showRouteFn _plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) -> routeClck u

clckMenuCallback :: ClckT ClckURL IO (String, [NamedLink])
clckMenuCallback =
    do adminURL <- showURL (Admin Console)
       return ("Clck", [NamedLink "Admin" adminURL])

clckInit :: ClckPlugins
         -> IO (Maybe Text)
clckInit plugins =
    do ~(Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       addRebacFragment plugins (pluginName clckPlugin) clckwrksFragment clckwrksRels
       -- rebac's platform administrators are the Administrator role's;
       -- clckwrks-cli keeps them so as roles change, this repairs any
       -- database it has not, such as one from before the sync existed
       acid <- cpsAcid <$> getPluginsSt plugins
       addRebacSync plugins (pluginName clckPlugin) $ \rebac ->
         syncAdministrators (acidProfileData acid) rebac "administrators at startup" >>= \r ->
           case r of
             Left errs -> ioError $ userError $ "rebac's administrators could not be made the Administrator role's: " ++ show errs
             Right (AdministratorsChange [] []) -> pure ()
             Right (AdministratorsChange added removed) ->
               putStrLn $ "rebac administrators at startup: " ++ show (length added) ++ " added, " ++ show (length removed) ++ " removed"
       addNavBarCallback plugins clckMenuCallback
       addHandler plugins (pluginName clckPlugin) (clckHandler clckShowFn)
       return Nothing

addClckAdminMenu :: ClckT url IO ()
addClckAdminMenu =
    do p <- plugins <$> get
       ~(Just clckShowURL) <- getPluginRouteFn p (pluginName clckPlugin)
       addAdminMenu ( "Profile"
                    , [ (Set.fromList [Administrator, Visitor], "Edit Your Profile"      , clckShowURL (Profile EditProfileData) [])
                      ]
                    )

       addAdminMenu ( "Clckwrks"
                    , [ (Set.singleton Administrator, "Console"       , clckShowURL (Admin Console)      [])
                      , (Set.singleton Administrator, "Edit Settings" , clckShowURL (Admin EditSettings) [])
                      , (Set.singleton Administrator, "Edit Nav Bar"  , clckShowURL (Admin EditNavBar)   [])
                      , (Set.singleton Administrator, "System Emails" , clckShowURL (Admin SystemEmails) [])
                      ]
                    )
       addAdminMenu ( "Access Control"
                    , [ (Set.singleton Administrator, "Schema"       , clckShowURL (Rebac SchemaPanel)      [])
                      , (Set.singleton Administrator, "Relations"    , clckShowURL (Rebac RelationsPanel)   [])
                      , (Set.singleton Administrator, "Relation Log" , clckShowURL (Rebac RelationLogPanel) [])
                      , (Set.singleton Administrator, "Check"        , clckShowURL (Rebac CheckPanel)       [])
                      ]
                    )

clckPlugin :: Plugin ClckURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
clckPlugin = Plugin
    { pluginName           = "clck"
    , pluginInit           = clckInit
    , pluginDepends        = []
    , pluginToPathSegments = toPathSegments
    , pluginPostHook       = addClckAdminMenu
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI clckPlugin

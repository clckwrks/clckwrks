{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Authenticate.Plugin where

import Clckwrks.Monad
import Clckwrks.Authenticate.Route (routeAuth)
import Clckwrks.Authenticate.URL  (AuthURL(..))
import Clckwrks.URL
import Control.Applicative         ((<$>))
import Control.Lens                ((^.))
import Control.Monad.State         (get)
import Control.Monad.Trans         (lift)
import Data.Acid                   (AcidState)
import Data.Monoid                 ((<>))
import Data.Text                   (Text)
import Data.Typeable               (Typeable)
import qualified Data.Text         as Text
import qualified Data.Set          as Set
import qualified Data.Text.Lazy as TL
import Happstack.Authenticate.Core (AuthenticateState, UserId, initAuthentication, getToken, userId)
import Happstack.Authenticate.Password.Route (initPassword)
import Happstack.Server
import System.FilePath             ((</>))
import Web.Plugins.Core            (Plugin(..), addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, initPlugin)
import Web.Routes

newtype AcidStateAuthenticate = AcidStateAuthenticate { acidStateAuthenticate :: AcidState AuthenticateState }
    deriving Typeable

authenticateHandler :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
                    -> (AuthURL -> [(Text, Maybe Text)] -> Text)
                    -> ClckPlugins
                    -> [Text]
                    -> ClckT ClckURL (ServerPartT IO) Response
authenticateHandler routeAuthenticate showAuthenticateURL _plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) -> routeAuth routeAuthenticate u -- ClckT $ withRouteT flattenURL $ unClckT $ 
  where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (AuthURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showAuthenticateURL u p

{-
clckMenuCallback :: ClckT ClckURL IO (String, [NamedLink])
clckMenuCallback =
    do adminURL <- showURL (Admin Console)
       return ("Clck", [NamedLink "Admin" adminURL])
-}
authenticateInit :: ClckPlugins
                 -> IO (Maybe Text)
authenticateInit plugins =
  do (Just authShowFn) <- getPluginRouteFn plugins (pluginName authenticatePlugin)
       -- addNavBarCallback plugins clckMenuCallback
       -- addHandler plugins (pluginName clckPlugin) (authenticateHandler clckShowFn)
     cc <- getConfig plugins
     let top' = fmap (\top -> top </> "_state") (clckTopDir cc)
         baseUri = case calcTLSBaseURI cc of
           Nothing  -> calcBaseURI cc
           (Just b) -> b
     (authCleanup, routeAuthenticate, authenticateState) <- initAuthentication top'
        [initPassword (baseUri <> authShowFn ResetPassword [] <> "/#") (Text.pack $ clckHostname cc)]
     addHandler plugins (pluginName authenticatePlugin) (authenticateHandler routeAuthenticate authShowFn)
     addPluginState plugins (pluginName authenticatePlugin) (AcidStateAuthenticate authenticateState)
     return Nothing
{-
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
                      , (Set.singleton Administrator, "Edit Nav Bar" , clckShowURL (Admin EditNavBar)   [])
                      ]
                    )
-}
authenticatePlugin :: Plugin AuthURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
authenticatePlugin = Plugin
    { pluginName       = "authenticate"
    , pluginInit       = authenticateInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return () -- addClckAdminMenu
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI authenticatePlugin

getUserId :: (Happstack m) => ClckT url m (Maybe UserId)
getUserId =
  do p <- plugins <$> get
     (Just (AcidStateAuthenticate authenticateState)) <- getPluginState p (pluginName authenticatePlugin)
     mToken <- getToken authenticateState
     case mToken of
       Nothing       -> return Nothing
       (Just (u, _)) -> return $ Just (u ^. userId)

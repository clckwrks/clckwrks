{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings, MultiParamTypeClasses #-}
module Clckwrks.Authenticate.Plugin where

import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Clckwrks.Monad
import Clckwrks.Acid               (GetAcidState(..), GetCoreState(..), GetEnableOpenId(..), acidCore, acidProfileData, coreFromAddress, coreLoginRedirect, coreReplyToAddress, coreSendmailPath, getAcidState)
import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))
import Clckwrks.Authenticate.Route (routeAuth)
import Clckwrks.Authenticate.URL   (AuthURL(..))
import Clckwrks.ProfileData.Acid   (HasRole(..))
import Clckwrks.ProfileData.Types  (Role(..))
import Clckwrks.Types              (NamedLink(..))
import Clckwrks.URL
import Control.Applicative         ((<$>))
import Control.Lens                ((^.))
import Control.Monad.Reader        (ask)
import Control.Monad.State         (get)
import Control.Monad.Trans         (MonadIO, lift)
import Data.Acid as Acid           (AcidState, query, openLocalStateFrom)
import Data.Maybe                  (isJust)
import Data.Monoid                 ((<>))
import qualified Data.Set          as Set
import Data.Text                   (Text)
import Data.Typeable               (Typeable)
import qualified Data.Text         as Text
import qualified Data.Set          as Set
import qualified Data.Text.Lazy as TL
import Data.UserId                  (UserId)
import Happstack.Authenticate.Core  (AuthenticateState, AuthenticateConfig(..), getToken, tokenUser, userId, usernamePolicy)
import Happstack.Authenticate.Route (initAuthentication)
import Happstack.Authenticate.Password.Core (PasswordConfig(..), initialPasswordState)
import Happstack.Authenticate.Password.Route (initPassword')
import Happstack.Authenticate.OpenId.Route (initOpenId)
import Happstack.Server
import System.FilePath             ((</>), combine)
import Web.Plugins.Core            (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin)
import Web.Routes

authenticateHandler
  :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
  -> (AuthURL -> [(Text, Maybe Text)] -> Text)
  -> ClckPlugins
  -> [Text]
  -> ClckT ClckURL (ServerPartT IO) Response
authenticateHandler routeAuthenticate showAuthenticateURL _plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) -> ClckT $ withRouteT flattenURL $ unClckT $ routeAuth routeAuthenticate u -- ClckT $ withRouteT flattenURL $ unClckT $
  where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (AuthURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showAuthenticateURL u p

authMenuCallback :: (AuthURL -> [(Text, Maybe Text)] -> Text)
                 -> ClckT ClckURL IO (String, [NamedLink])
authMenuCallback authShowFn =
       return ("Authenticate", [NamedLink "Login" (authShowFn Login [])])

addAuthAdminMenu :: ClckT url IO ()
addAuthAdminMenu =
    do p <- plugins <$> get
       ~(Just authShowURL) <- getPluginRouteFn p (pluginName authenticatePlugin)
       addAdminMenu ("Authentication", [(Set.fromList [Visitor]      , "Change Password"     , authShowURL ChangePassword [])])
       addAdminMenu ("Authentication", [(Set.fromList [Administrator], "OpenId Realm"        , authShowURL OpenIdRealm    [])])
       addAdminMenu ("Authentication", [(Set.fromList [Administrator], "Authentication Modes", authShowURL AuthModes      [])])
       addAdminMenu ("Authentication", [(Set.fromList [Administrator], "View Users"          , authShowURL ViewUsers      [])])

authenticateInit
  :: ClckPlugins
  -> IO (Maybe Text)
authenticateInit plugins =
  do ~(Just authShowFn) <- getPluginRouteFn plugins (pluginName authenticatePlugin)
     addNavBarCallback plugins (authMenuCallback authShowFn)
     -- addHandler plugins (pluginName clckPlugin) (authenticateHandler clckShowFn)
     cc <- getConfig plugins
     acid <- cpsAcid <$> getPluginsSt plugins
     let basePath = maybe "_state" (\top -> top </> "_state") (clckTopDir cc)
         baseUri = case calcTLSBaseURI cc of
           Nothing  -> calcBaseURI cc
           (Just b) -> b
     cs <- Acid.query (acidCore acid) GetCoreState
     let authenticateConfig = AuthenticateConfig {
                                _isAuthAdmin          = \uid -> Acid.query (acidProfileData acid) (HasRole uid (Set.singleton Administrator))
                              , _usernameAcceptable   = usernamePolicy
                              , _requireEmail         = True
                              , _systemFromAddress    = cs ^. coreFromAddress
                              , _systemReplyToAddress = cs ^. coreReplyToAddress
                              , _systemSendmailPath   = cs ^. coreSendmailPath
                              , _postLoginRedirect    = cs ^. coreLoginRedirect
                              , _createUserCallback   = Nothing
                              }
         passwordConfig = PasswordConfig {
                            _resetLink = authShowFn ResetPassword [] <> "/#" -- we should not need 'baseUri <>', but at some point we did
                          , _domain = Text.pack $ clckHostname cc
                          , _passwordAcceptable = const Nothing
                          }

     passwordState <- openLocalStateFrom (combine (combine basePath "authenticate") "password") initialPasswordState
     passwordConfigTV <- atomically $ newTVar passwordConfig
     (authCleanup, routeAuthenticate, authenticateState, authenticateConfigTV) <- initAuthentication (Just basePath) authenticateConfig
        ((initPassword' passwordConfigTV passwordState) : if True then [ initOpenId ] else [])
     addHandler     plugins (pluginName authenticatePlugin) (authenticateHandler routeAuthenticate authShowFn)
     addPluginState plugins (pluginName authenticatePlugin) (AuthenticatePluginState authenticateState passwordState authenticateConfigTV passwordConfigTV)
     addCleanup plugins Always authCleanup
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
    { pluginName           = "authenticate"
    , pluginInit           = authenticateInit
    , pluginDepends        = []
    , pluginToPathSegments = toPathSegments
    , pluginPostHook       = addAuthAdminMenu
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI authenticatePlugin

getUserId :: (Happstack m) => ClckT url m (Maybe UserId)
getUserId =
  do p <- plugins <$> get
     ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     mToken <- getToken (acidStateAuthenticate aps)
     case mToken of
       Nothing       -> return Nothing
       (Just (token, _)) -> return $ Just (token ^. tokenUser ^. userId)


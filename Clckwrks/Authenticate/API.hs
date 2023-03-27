{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Clckwrks.Authenticate.API
       ( Username(..)
       , getEmail
       , getUser
       , getUsername
       , insecureUpdateUser
       , setCreateUserCallback
       , setSignupPluginURL
       ) where

import Clckwrks.Authenticate.Plugin (authenticatePlugin, authenticatePluginLoader)
import Clckwrks.Authenticate.Monad  (AuthenticatePluginState(..))
import Clckwrks.Monad               (Clck, ClckPlugins, plugins)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (modifyTVar')
import Control.Monad                (join)
import Control.Monad.State          (get)
import Control.Monad.Trans          (liftIO)
import Data.Acid as Acid            (AcidState, query, update)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import Data.Maybe                   (maybe)
import Data.Monoid                  (mempty)
import Data.Text                    (Text)
import Data.UserId                  (UserId)
import Happstack.Authenticate.Core  (Email(..), User(..), Username(..))
import Happstack.Authenticate.Handlers  (AuthenticateConfig(_createUserCallback), GetUserByUserId(..), UpdateUser(..))
import Web.Plugins.Core             (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin, modifyPluginState')

getUser :: UserId -> Clck url (Maybe User)
getUser uid =
  do p <- plugins <$> get
     ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.query (acidStateAuthenticate aps) (GetUserByUserId uid)

-- | Update an existing 'User'. Must already have a valid 'UserId'.
--
-- no security checks are performed to ensure that the caller is
-- authorized to change data for the 'User'.
insecureUpdateUser :: User -> Clck url ()
insecureUpdateUser user =
  do p <- plugins <$> get
     ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.update (acidStateAuthenticate aps) (UpdateUser user)

getUsername :: UserId -> Clck url (Maybe Username)
getUsername uid =
  do mUser <- getUser uid
     pure $ _username <$> mUser

getEmail :: UserId -> Clck url (Maybe Email)
getEmail uid =
  do mUser <- getUser uid
     pure $ join $ _email <$> mUser

setCreateUserCallback :: ClckPlugins -> Maybe (User -> IO ()) -> IO ()
setCreateUserCallback p mcb =
  do ~(Just aps) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ atomically $ modifyTVar' (apsAuthenticateConfigTV aps) $ (\ac -> ac { _createUserCallback = mcb })
     pure ()

setSignupPluginURL :: ClckPlugins
                   -> Text
                   -> Text
                   -> IO ()
setSignupPluginURL plugins pn pu =
  do modifyPluginState' plugins (pluginName authenticatePlugin) $ \aps ->
       aps { apsSignupPluginURLs = Map.insert pn pu (apsSignupPluginURLs aps) }
     authenticatePluginLoader plugins
     pure ()


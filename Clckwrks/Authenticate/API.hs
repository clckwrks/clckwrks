{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Clckwrks.Authenticate.API
       ( Username(..)
       , getEmail
       , getUser
       , getUsername
       ) where

import Clckwrks.Monad               (Clck, plugins)
import Control.Monad                (join)
import Control.Monad.State          (get)
import Control.Monad.Trans          (liftIO)
import Clckwrks.Authenticate.Plugin (AcidStateAuthenticate(..), authenticatePlugin)
import Data.Acid as Acid            (AcidState, query, update)
import Data.Maybe                   (maybe)
import Data.Monoid                  (mempty)
import Data.Text                    (Text)
import Data.UserId                  (UserId)
import Happstack.Authenticate.Core  (GetUserByUserId(..), Email(..), UpdateUser(..), User(..), Username(..))
import Web.Plugins.Core             (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin)

getUser :: UserId -> Clck url (Maybe User)
getUser uid =
  do p <- plugins <$> get
     ~(Just (AcidStateAuthenticate authenticateState)) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.query authenticateState (GetUserByUserId uid)

-- | Update an existing 'User'. Must already have a valid 'UserId'.
--
-- no security checks are performed to ensure that the caller is
-- authorized to change data for the 'User'.
insecureUpdateUser :: User -> Clck url ()
insecureUpdateUser user =
  do p <- plugins <$> get
     ~(Just (AcidStateAuthenticate authenticateState)) <- getPluginState p (pluginName authenticatePlugin)
     liftIO $ Acid.update authenticateState (UpdateUser user)

getUsername :: UserId -> Clck url (Maybe Username)
getUsername uid =
  do mUser <- getUser uid
     pure $ _username <$> mUser

getEmail :: UserId -> Clck url (Maybe Email)
getEmail uid =
  do mUser <- getUser uid
     pure $ join $ _email <$> mUser

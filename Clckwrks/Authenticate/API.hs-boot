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

-- import Clckwrks.Authenticate.Plugin (authenticatePlugin, authenticatePluginLoader)
import Clckwrks.Authenticate.Monad  (AuthenticatePluginState(..))
import Clckwrks.Monad               (ClckT, ClckPlugins, plugins)
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
import Happstack.Server             (Happstack)
import Happstack.Authenticate.Core  (Email(..), User(..), Username(..))
import Happstack.Authenticate.Handlers  (AuthenticateConfig(_createUserCallback), GetUserByUserId(..), UpdateUser(..))
import Web.Plugins.Core             (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin, modifyPluginState')

getUser :: (Happstack m) => UserId -> ClckT url m (Maybe User)
insecureUpdateUser :: (Happstack m) => User -> ClckT url m ()
getUsername :: (Happstack m) => UserId -> ClckT url m (Maybe Username)
getEmail :: (Happstack m) => UserId -> ClckT url m (Maybe Email)
setCreateUserCallback :: ClckPlugins -> Maybe (User -> IO ()) -> IO ()
setSignupPluginURL :: ClckPlugins
                   -> Text
                   -> Text
                   -> IO ()


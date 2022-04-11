{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings, MultiParamTypeClasses #-}
module Clckwrks.Authenticate.Monad where

import Clckwrks.Acid               (GetAcidState(..), GetCoreState(..), GetEnableOpenId(..), acidCore, acidProfileData, coreFromAddress, coreReplyToAddress, coreSendmailPath, getAcidState)
import Clckwrks.Monad
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.State         (get)
import Control.Monad.Trans         (MonadIO, lift)
import Data.Acid as Acid           (AcidState, query)
import Data.Typeable               (Typeable)
import Happstack.Authenticate.Core (AuthenticateState, AuthenticateConfig(..), User, getToken, tokenUser, userId, usernamePolicy)
import Happstack.Authenticate.Password.Core (PasswordConfig, PasswordState)
import Web.Plugins.Core            (getPluginState)

data AuthenticatePluginState = AuthenticatePluginState
  { acidStateAuthenticate   :: AcidState AuthenticateState
  , acidStatePassword       :: AcidState PasswordState
  , apsAuthenticateConfigTV :: TVar AuthenticateConfig
  , apsPasswordConfigTV     :: TVar PasswordConfig
  }
  deriving Typeable

instance (Functor m, MonadIO m) => GetAcidState (ClckT url m) AuthenticateState where
    getAcidState =
      do p <- plugins <$> get
         ~(Just aps) <- getPluginState p "authenticate"
         pure (acidStateAuthenticate aps)

instance (Functor m, MonadIO m) => GetAcidState (ClckT url m) PasswordState where
    getAcidState =
      do p <- plugins <$> get
         ~(Just aps) <- getPluginState p "authenticate"
         pure (acidStatePassword aps)


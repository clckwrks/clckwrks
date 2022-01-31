{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings, MultiParamTypeClasses #-}
module Clckwrks.Authenticate.Monad where

import Clckwrks.Acid               (GetAcidState(..), GetCoreState(..), GetEnableOpenId(..), acidCore, acidProfileData, coreFromAddress, coreReplyToAddress, coreSendmailPath, getAcidState)
import Clckwrks.Monad
import Control.Monad.State         (get)
import Control.Monad.Trans         (MonadIO, lift)
import Data.Acid as Acid           (AcidState, query)
import Data.Typeable               (Typeable)
import Happstack.Authenticate.Core (AuthenticateState, AuthenticateConfig(..), getToken, tokenUser, userId, usernamePolicy)
import Happstack.Authenticate.Password.Core (PasswordState)
import Web.Plugins.Core            (getPluginState)

data AcidStateAuthenticate = AcidStateAuthenticate
  { acidStateAuthenticate :: AcidState AuthenticateState
  , acidStatePassword     :: AcidState PasswordState                                                }
  deriving Typeable

instance (Functor m, MonadIO m) => GetAcidState (ClckT url m) AuthenticateState where
    getAcidState =
      do p <- plugins <$> get
         ~(Just (AcidStateAuthenticate authenticateState _)) <- getPluginState p "authenticate"
         pure authenticateState

instance (Functor m, MonadIO m) => GetAcidState (ClckT url m) PasswordState where
    getAcidState =
      do p <- plugins <$> get
         ~(Just (AcidStateAuthenticate _ passwordState)) <- getPluginState p "authenticate"
         pure passwordState


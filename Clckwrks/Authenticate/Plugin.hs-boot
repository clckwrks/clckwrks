{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Authenticate.Plugin where

import Happstack.Authenticate.Core (UserId)
import Clckwrks.Authenticate.URL (AuthURL)
import Clckwrks.Monad
import Clckwrks.URL
import Happstack.Server
import Web.Plugins.Core            (Plugin(..), addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, initPlugin)

authenticatePlugin :: Plugin AuthURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt

getUserId :: (Happstack m) => ClckT url m (Maybe UserId)

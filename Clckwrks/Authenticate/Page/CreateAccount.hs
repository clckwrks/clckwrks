{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.CreateAccount where

import Control.Applicative ((<$>))
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import Clckwrks.URL (ClckURL(..))
import Clckwrks.Authenticate.URL
import Control.Monad.State (get)
import Happstack.Server (Response, ServerPartT)
import HSP
import Language.Haskell.HSX.QQ (hsx)

createAccountPage :: ClckT ClckURL (ServerPartT IO) Response
createAccountPage =
  do plugins <- plugins <$> get
     themeTemplate plugins (ThemeStyleId 0) "Create New Account" () [hsx|
      <div class="happstack-authenticate happstack-authenticate-create-account">
       <h2>Create A New Account</h2>
       <up-signup-password />
      </div> |]

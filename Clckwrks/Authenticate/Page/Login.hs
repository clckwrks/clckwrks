{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.Login where

import Control.Applicative ((<$>))
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import {-# SOURCE #-} Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.URL
import Clckwrks.URL (ClckURL)
import Control.Monad.State (get)
import Happstack.Server (Response, ServerPartT)
import HSP
import Language.Haskell.HSX.QQ (hsx)
import Web.Plugins.Core        (pluginName, getPluginRouteFn)

loginPage :: ClckT ClckURL (ServerPartT IO) Response
loginPage =
  do p <- plugins <$> get
     ~(Just authShowURL) <- getPluginRouteFn p (pluginName authenticatePlugin)
     themeTemplate p (ThemeStyleId 0) "Login" () [hsx|
      <div class="happstack-authenticate happstack-authenticate-login">
        <h2>Login</h2>
        <up-login />
        <p><a href=(authShowURL ForgotPassword [])>Forgot Password?</a></p>
        <p><a href=(authShowURL CreateAccount [])>Create new account</a></p>

        <up-logout />
       </div>
       |]

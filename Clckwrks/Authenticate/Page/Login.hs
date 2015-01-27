{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.Login where

import Control.Applicative ((<$>))
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import Clckwrks.URL (ClckURL(..))
import Clckwrks.Authenticate.URL
import Control.Monad.State (get)
import Happstack.Server (Response, ServerPartT)
import HSP
import Language.Haskell.HSX.QQ (hsx)

loginPage :: ClckT ClckURL (ServerPartT IO) Response
loginPage =
  do plugins <- plugins <$> get
     themeTemplate plugins (ThemeStyleId 0) "Login" () [hsx|
      <%>
       <div up-authenticated=False>
        <h2>Login</h2>
       </div>
       <div up-authenticated=True>
        <h2>Logout</h2>
        <p>You have successfully logged in! Click the link below to logout.</p>
       </div>
       <up-login-inline />
       <div up-authenticated=False>
        <h2>Forgotten Password?</h2>
        <p>Forgot your password? Request a reset link via email!</p>
        <up-request-reset-password />
       </div>
       <h2>Create A New Account</h2>
       <up-signup-password />
      </%> |]

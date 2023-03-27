{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.ForgotPassword where

import Control.Applicative ((<$>))
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import Clckwrks.URL (ClckURL(..))
import Clckwrks.Authenticate.URL
import Control.Monad.State (get)
import Happstack.Server (Response, ServerPartT)
import HSP
import Language.Haskell.HSX.QQ (hsx)

forgotPasswordPage :: ClckT ClckURL (ServerPartT IO) Response
forgotPasswordPage =
  do plugins <- plugins <$> get
     themeTemplate plugins (ThemeStyleId 0) "Forgot Password" () [hsx|
      <div class="happstack-authenticate happstack-authenticate-forgot-password">
        <h2>Forgotten Password?</h2>
        <p>Forgot your password? Request a reset link via email!</p>
        <up-request-reset-password />
      </div> |]

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.ResetPassword where

import Control.Applicative ((<$>))
import Clckwrks.Monad (ClckT, ThemeStyleId(..), plugins, themeTemplate)
import Clckwrks.URL (ClckURL(..))
import Clckwrks.Authenticate.URL
import Control.Monad.State (get)
import Happstack.Server (Response, ServerPartT)
import HSP
import Language.Haskell.HSX.QQ (hsx)

resetPasswordPage :: ClckT ClckURL (ServerPartT IO) Response
resetPasswordPage =
  do plugins <- plugins <$> get
     themeTemplate plugins (ThemeStyleId 0) "Reset Password" () [hsx|
      <%>
        <h2>Reset Password</h2>
        <up-reset-password />
      </%> |]

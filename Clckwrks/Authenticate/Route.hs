{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Authenticate.Route where

import Control.Applicative         ((<$>))
import Clckwrks.Monad              (ClckT, plugins)
import Clckwrks.Authenticate.URL   (AuthURL(..))
import Clckwrks.Authenticate.Page.Login (loginPage)
import Clckwrks.Authenticate.Page.ChangePassword (changePasswordPanel)
import Clckwrks.Authenticate.Page.ResetPassword  (resetPasswordPage)
import Clckwrks.Authenticate.Page.OpenIdRealm    (openIdRealmPanel)
import Clckwrks.URL                (ClckURL)
import Control.Monad.State         (get)
import Control.Monad.Trans         (lift)
import Happstack.Authenticate.Core (AuthenticateURL)
import Happstack.Server            (Response, ServerPartT)
import Web.Routes                  (RouteT, askRouteFn, runRouteT)
import Web.Plugins.Core            (getPluginRouteFn, pluginName)

-- | routeAuth
routeAuth :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
          -> AuthURL
          -> ClckT ClckURL (ServerPartT IO) Response
routeAuth routeAuthenticate u =
  case u of
    (Auth authenticateURL) ->
      do p <- plugins <$> get
         ~(Just authShowFn) <- getPluginRouteFn p "authenticate"
         lift $ runRouteT routeAuthenticate (authShowFn . Auth) authenticateURL
    Login          -> loginPage
    ResetPassword  -> resetPasswordPage
    ChangePassword -> changePasswordPanel
    OpenIdRealm    -> openIdRealmPanel


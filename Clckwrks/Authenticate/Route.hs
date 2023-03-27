{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Authenticate.Route where

import Control.Applicative         ((<$>))
import Clckwrks.Monad              (ClckT(..), plugins)
import Clckwrks.Authenticate.URL   (AuthURL(..))
import Clckwrks.Authenticate.Page.AuthModes (authModesPage)
import Clckwrks.Authenticate.Page.Login (loginPage)
import Clckwrks.Authenticate.Page.ChangePassword (changePasswordPanel)
import Clckwrks.Authenticate.Page.CreateAccount  (createAccountPage)
import Clckwrks.Authenticate.Page.ForgotPassword (forgotPasswordPage)
import Clckwrks.Authenticate.Page.ResetPassword  (resetPasswordPage)
import Clckwrks.Authenticate.Page.OpenIdRealm    (openIdRealmPanel)
import Clckwrks.Authenticate.Page.ViewUsers      (viewUsers)
import Clckwrks.ProfileData.API    (Role(..), requiresRole_)
import Clckwrks.URL                (ClckURL)
-- import Clckwrks.Plugin             (clckPlugin)
import Control.Monad.State         (get)
import Control.Monad.Trans         (lift)
import qualified Data.Set          as Set
import Happstack.Authenticate.Core (AuthenticateURL)
import Happstack.Server            (Happstack, Response, ServerPartT)
import Web.Routes                  (RouteT(..), askRouteFn, runRouteT)
import Web.Plugins.Core            (getPluginRouteFn, pluginName)

-- | routeAuth
-- there is much craziness here. This should be more like clckwrks-plugin-page or something
routeAuth :: (AuthenticateURL -> RouteT AuthenticateURL (ServerPartT IO) Response)
          -> AuthURL
          -> ClckT AuthURL (ServerPartT IO) Response
routeAuth routeAuthenticate u' =
  do u <- checkAuth u'
     p <- plugins <$> get
     ~(Just clckShowFn) <- getPluginRouteFn p "clck" -- (pluginName clckPlugin) -- a mildly dangerous hack to avoid circular depends
     let withClckURL m =  ClckT $ RouteT $ \_ -> unRouteT (unClckT m) clckShowFn
     case u of
       (Auth authenticateURL) ->
         do p <- plugins <$> get
            ~(Just authShowFn) <- getPluginRouteFn p "authenticate"
            lift $ runRouteT routeAuthenticate (authShowFn . Auth) authenticateURL
       Login          -> withClckURL loginPage
       CreateAccount  -> withClckURL createAccountPage
       ForgotPassword -> withClckURL forgotPasswordPage
       ResetPassword  -> withClckURL resetPasswordPage
       ChangePassword -> withClckURL changePasswordPanel
       OpenIdRealm    -> withClckURL openIdRealmPanel
       AuthModes      -> authModesPage u
       ViewUsers      -> withClckURL viewUsers

checkAuth :: (Happstack m, Monad m) =>
             AuthURL
          -> ClckT AuthURL m AuthURL
checkAuth url =
  do p <- plugins <$> get
     ~(Just clckShowFn) <- getPluginRouteFn p "clck" -- (pluginName clckPlugin) -- a mildly dangerous hack to avoid circular depends
     let requiresRole = requiresRole_ clckShowFn
     case url of
       (Auth {})      -> pure url
       CreateAccount  -> pure url
       Login          -> pure url
       ResetPassword  -> pure url
       AuthModes      -> requiresRole (Set.fromList [Administrator]) url
       ChangePassword -> requiresRole (Set.fromList [Visitor]) url
       ForgotPassword -> pure url
       OpenIdRealm    -> requiresRole (Set.fromList [Administrator]) url
       ViewUsers      -> requiresRole (Set.fromList [Administrator]) url

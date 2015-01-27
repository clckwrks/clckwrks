{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Admin.Template where

import Control.Applicative     ((<$>))
import Control.Monad.Trans     (lift)
import Clckwrks.Acid           (GetSiteName(..))
import Clckwrks.Monad          (ClckT(..), ClckState(adminMenus), plugins, query)
import Clckwrks.URL            (ClckURL(JS))
import Clckwrks.JS.URL         (JSURL(..))
import {-# SOURCE #-} Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.URL    (AuthURL(Auth))
import Clckwrks.ProfileData.API (getUserRoles)
import Clckwrks.ProfileData.Types (Role)
import Control.Monad.State     (get)
import Data.Maybe              (mapMaybe, fromMaybe)
import Data.Text.Lazy          (Text)
import qualified               Data.Text as T
import Data.Set                (Set)
import qualified Data.Set      as Set
import Happstack.Authenticate.Core (AuthenticateURL(Controllers))
import Happstack.Server        (Happstack, Response, toResponse)
import HSP.XMLGenerator
import HSP.XML                 (XML, fromStringLit)
import Language.Haskell.HSX.QQ (hsx)
import Web.Plugins.Core        (pluginName, getPluginRouteFn)

template ::
    ( Happstack m
    , EmbedAsChild (ClckT url m) headers
    , EmbedAsChild (ClckT url m) body
    ) => String -> headers -> body -> ClckT url m Response
template title headers body = do
   siteName <- (fromMaybe "Your Site") <$> query GetSiteName
   p <- plugins <$> get
   (Just authShowURL) <- getPluginRouteFn p (pluginName authenticatePlugin)
   (Just clckShowURL) <- getPluginRouteFn p "clck"
--   let passwordShowURL u = authShowURL (Auth (AuthenticationMethods $ Just (passwordAuthenticationMethod, toPathSegments u))) []
   toResponse <$> (unXMLGenT $ [hsx|
    <html>
     <head>
      <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"        rel="stylesheet" media="screen" />
--      <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.2.2/css/bootstrap-responsive.css" rel="stylesheet" />
      <link type="text/css" href="/static/admin.css" rel="stylesheet" />
      <script type="text/javascript" src="/jquery/jquery.js" ></script>
      <script type="text/javascript" src="/json2/json2.js" ></script>
      <script type="text/javascript" src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js" ></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular.min.js"></script>
      <script src="//ajax.googleapis.com/ajax/libs/angularjs/1.2.24/angular-route.min.js"></script>
--      <script src=(passwordShowURL UsernamePasswordCtrl)></script>
      <script src=(clckShowURL (JS ClckwrksApp) [])></script>
      <script src=(authShowURL (Auth Controllers) [])></script>
      <title><% title %></title>
      <% headers %>
     </head>
     <body ng-app="clckwrksApp" ng-controller="UsernamePasswordCtrl">
      <div class="navbar">
       <div class="navbar-inner">
        <div class="container-fluid">
         <a href="/" class="brand">Back to <% siteName %></a>
        </div>
       </div>
      </div>

      <div class="container-fluid">
       <div class="row-fluid">
        <div class="span2">
         <% sidebar %>
        </div>
        <div class="span10">
         <% body %>
        </div>
       </div>
      </div>
     </body>
    </html>
 |])

emptyTemplate ::
    ( Happstack m
    , EmbedAsChild (ClckT url m) headers
    , EmbedAsChild (ClckT url m) body
    ) => String -> headers -> body -> ClckT url m Response
emptyTemplate title headers body = do
   siteName <- (fromMaybe "Your Site") <$> query GetSiteName
   toResponse <$> (unXMLGenT $ [hsx|
    <html>
     <head>
      <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.2.2/css/bootstrap.min.css"        rel="stylesheet" media="screen" />
      <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.2.2/css/bootstrap-responsive.css" rel="stylesheet" />
      <link type="text/css" href="/static/admin.css" rel="stylesheet" />
      <script type="text/javascript" src="/jquery/jquery.js" ></script>
      <script type="text/javascript" src="/json2/json2.js" ></script>
      <script type="text/javascript" src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.2.2/js/bootstrap.min.js" ></script>

      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <div class="navbar">
       <div class="navbar-inner">
        <div class="container-fluid">
         <div class="brand"><% siteName %></div>
        </div>
       </div>
      </div>

      <div class="container-fluid">
       <div class="row-fluid">
        <div class="span2">
--         <% sidebar %>
        </div>
        <div class="span10">
         <% body %>
        </div>
       </div>
      </div>
     </body>
    </html> |])

sidebar :: (Happstack m) => XMLGenT (ClckT url m) XML
sidebar = adminMenuXML

adminMenuXML :: (Happstack m) => XMLGenT (ClckT url m) XML
adminMenuXML =
    do allMenus <- adminMenus <$> get
       usersMenus <- filterByRole allMenus
       [hsx| <div class="well">
        <ul class="nav nav-list">
         <% mapM mkMenu usersMenus %>
        </ul>
       </div> |]
    where
--       filterByRole :: [(T.Text, [(Set Role, T.Text, T.Text)])] -> [(T.Text, [(Set Role, T.Text, T.Text)])]
      filterByRole menus =
          do userRoles <- lift getUserRoles
             return $ mapMaybe (sectionFilter userRoles) menus
      sectionFilter userRoles (title, items) =
          case filter (itemFilter userRoles) items of
            [] -> Nothing
            items' -> Just (title, items')
      itemFilter userRoles (visibleRoles, _, _) = not (Set.null (Set.intersection userRoles visibleRoles))

--      mkMenu :: (Functor m, Monad m) => (T.Text, [(Set Role, T.Text, T.Text)]) -> XMLGenT (ClckT url m) XML
      mkMenu (category, links) = [hsx|
          <%>
           <li class="nav-header"><% category %></li>
           <% mapM mkLink links %>
          </%> |]
      mkLink :: (Functor m, Monad m) => (Set Role, T.Text, T.Text) -> XMLGenT (ClckT url m) XML
      mkLink (_visible, title, url) = [hsx|
          <li><a href=url><% title %></a></li>
        |]

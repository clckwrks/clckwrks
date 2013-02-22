{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.Template where

import Clckwrks
import Control.Monad.State (get)
import Data.Maybe          (mapMaybe)
import qualified           Data.Text as T
import Data.Set            (Set)
import qualified Data.Set  as Set


template ::
    ( Happstack m
    , EmbedAsChild (ClckT url m) headers
    , EmbedAsChild (ClckT url m) body
    ) => String -> headers -> body -> ClckT url m Response
template title headers body =
   toResponse <$> (unXMLGenT $
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
         <a href="/" class="brand">Back to Your Site</a>
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
    </html>)

sidebar :: (Happstack m) => XMLGenT (ClckT url m) XML
sidebar = adminMenuXML

adminMenuXML :: (Happstack m) => XMLGenT (ClckT url m) XML
adminMenuXML =
    do allMenus <- adminMenus <$> get
       usersMenus <- filterByRole allMenus
       <div class="well">
        <ul class="nav nav-list">
         <% mapM mkMenu usersMenus %>
        </ul>
       </div>
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
      mkMenu (category, links) =
          <%>
           <li class="nav-header"><% category %></li>
           <% mapM mkLink links %>
          </%>
      mkLink :: (Functor m, Monad m) => (Set Role, T.Text, T.Text) -> XMLGenT (ClckT url m) XML
      mkLink (_visible, title, url) =
          <li><a href=url><% title %></a></li>

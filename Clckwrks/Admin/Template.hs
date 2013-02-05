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
      <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap.min.css"        rel="stylesheet" media="screen" />
      <link href="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap-responsive.css" rel="stylesheet" />
      <link type="text/css" href="/static/admin.css" rel="stylesheet" />
      <script type="text/javascript" src="/jquery/jquery.js" ></script>
      <script type="text/javascript" src="/json2/json2.js" ></script>
      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <% sidebar %>
      <div id="admin-body">
       <% body %>
      </div>
     </body>
    </html>)

sidebar :: (Happstack m) => XMLGenT (ClckT url m) XML
sidebar =
    <div id="admin-sidebar">
      <% adminMenuXML %>
    </div>

adminMenuXML :: (Happstack m) => XMLGenT (ClckT url m) XML
adminMenuXML =
    do allMenus <- adminMenus <$> get
       usersMenus <- filterByRole allMenus
       <ul id="admin-menu">
          <% mapM mkMenu usersMenus %>
        </ul>
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

      mkMenu :: (Functor m, Monad m) => (T.Text, [(Set Role, T.Text, T.Text)]) -> XMLGenT (ClckT url m) XML
      mkMenu (category, links) =
          <li class="admin-menu-category"><span class="admin-menu-category-title"><% category %></span>
              <ul id="admin-menu-links">
               <% mapM mkLink links %>
              </ul>
          </li>
      mkLink :: (Functor m, Monad m) => (Set Role, T.Text, T.Text) -> XMLGenT (ClckT url m) XML
      mkLink (visible, title, url) =
          <li class="admin-menu-link"><a href=url><% title %></a></li>

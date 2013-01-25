{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.Template where

import Clckwrks hiding (mapM, sequence)
import Control.Arrow       (second)
import Control.Monad.State (get)
import Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Instances
import Prelude hiding (mapM, sequence)

import Data.Monoid
import Data.Foldable
import Data.Traversable

template ::
    ( Functor m
    , Monad m
    , EmbedAsChild (ClckT url m) headers
    , EmbedAsChild (ClckT url m) body
    ) => String -> headers -> body -> ClckT url m Response
template title headers body =
   toResponse <$> (unXMLGenT $
    <html>
     <head>
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

sidebar :: (Functor m, Monad m) => XMLGenT (ClckT url m) XML
sidebar =
    <div id="admin-sidebar">
      <% adminMenuXML %>
    </div>

adminMenuXML :: (Functor m, Monad m) => XMLGenT (ClckT url m) XML
adminMenuXML =
    do menu <- adminMenus <$> get
       <ul id="admin-menu">
          <% mapM mkMenu menu %>
        </ul>
    where
      mkMenu :: (Functor m, Monad m) => (T.Text, [(T.Text, T.Text)]) -> XMLGenT (ClckT url m) XML
      mkMenu (category, links) =
          <li class="admin-menu-category"><span class="admin-menu-category-title"><% category %></span>
              <ul id="admin-menu-links">
               <% mapM mkLink links %>
              </ul>
          </li>
      mkLink :: (Functor m, Monad m) => (T.Text, T.Text) -> XMLGenT (ClckT url m) XML
      mkLink (title, url) =
          <li class="admin-menu-link"><a href=url><% title %></a></li>

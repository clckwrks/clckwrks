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

m :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
m f x = l $ second f x

l :: (Monad m) => (a, m b) -> m (a, b)
l (a, m) =
    do b <- m
       return (a ,b)

instance (Monoid a) => Monad ((,) a) where
    return b = (mempty, b)
    (a, b) >>= f = let (a', b') = f b in (a `mappend` a', b')

instance Foldable ((,) a) where
    fold = snd
    foldMap f (b, a) = f a

instance Traversable ((,) a) where
    traverse f (c, a) = fmap (\b -> (c, b)) $ f a
    sequenceA (c, fa) = fmap (\a -> (c, a)) fa
    mapM f (c, a) =
        do b <- f a
           return (c, b)
    sequence (a, m) = do b <- m
                         return (a, b)

defaultAdminMenu :: (Monad m) => ClckT ClckURL m [(Text, [(Text, Text)])]
defaultAdminMenu =
    do links <- sequence $ map sequence $ map (second (showURL . Admin))
                 [ (fromString "Console"         , Console)
                 , (fromString "Edit Settings"   , EditSettings)
                 , (fromString "Edit Feed Config", EditFeedConfig)
                 , (fromString "Edit Page"       , Pages)
                 , (fromString "New Page/Post"   , NewPage)
                 , (fromString "Edit Menu"       , EditMenu)
                 ]
       return [(fromString "Admin", links)]

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

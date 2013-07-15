{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.BasicTemplate (basicTemplate) where

import Control.Applicative ((<$>))
import Clckwrks.Monad
import Data.Text.Lazy (Text)
import Happstack.Server (Response, toResponse)
import HSP.XMLGenerator
import HSP.XML

basicTemplate ::
    ( Functor m
    , Monad m
    , EmbedAsChild (ClckT url m) headers
    , EmbedAsChild (ClckT url m) body
    ) => Text -> headers -> body -> ClckT url m Response
basicTemplate title headers body =
   toResponse <$> (unXMLGenT $
    <html>
     <head>
      <link type="text/css" href="/static/style.css" rel="stylesheet" />
      <script type="text/javascript" src="/jquery/jquery.js" ></script>
      <script type="text/javascript" src="/json2/json2.js" ></script>
      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <div id="body">
       <% body %>
      </div>
     </body>
    </html>)

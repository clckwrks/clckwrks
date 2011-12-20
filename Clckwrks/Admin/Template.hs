{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.Template where

import Clckwrks

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
      <link type="text/css" href="/static/style.css" rel="stylesheet" />
      <script type="text/javascript" src="/jquery/jquery.js" ></script>
      <script type="text/javascript" src="/json2/json2.js" ></script>
      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <% body %>
     </body>
    </html>)

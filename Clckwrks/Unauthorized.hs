{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Unauthorized
    ( unauthorizedPage
    ) where

import Control.Applicative ((<$>))
-- import HSP
import Data.Text.Lazy
import Happstack.Server (Happstack, Response, ToMessage, toResponse, unauthorized)
import HSP.XML
import HSP.XMLGenerator

unauthorizedPage ::
    ( Happstack m
    , XMLGenerator m
    , StringType m ~ Text
    , EmbedAsChild m msg
    , ToMessage (XMLType m)
    ) => msg -> m Response
unauthorizedPage msg =
    do unauthorized ()
       toResponse <$> (unXMLGenT $
         <html>
          <head>
            <title>Unauthorized</title>
          </head>
          <body>
           <div>
            <h1>Unauthorized</h1>
            <p><% msg %></p>
           </div>
          </body>
          </html>)

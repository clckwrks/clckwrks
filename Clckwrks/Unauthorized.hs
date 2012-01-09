{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Unauthorized 
    ( unauthorizedPage
    ) where

import Control.Applicative ((<$>))
import HSP
import Happstack.Server (Happstack, Response, ToMessage, toResponse, unauthorized)
import qualified HSX.XMLGenerator as HSX

unauthorizedPage :: 
    ( Happstack m
    , XMLGenerator m
    , EmbedAsChild m msg
    , ToMessage (HSX.XML m)
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

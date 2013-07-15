{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Admin.Console where

import Clckwrks                (AdminURL(..), Clck, Response)
import Clckwrks.Admin.Template (template)
import Data.Text.Lazy          (Text)
import HSP.XMLGenerator
import HSP.XML

consolePage :: Clck AdminURL Response
consolePage =
    do template "Administration" () $
         <div>
          <p>Enter a world of excitement.</p>
         </div>


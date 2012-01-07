{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.Console where

import Clckwrks                (AdminURL(..), Clck, Response)
import Clckwrks.Admin.Template (template)
import HSP

consolePage :: Clck AdminURL Response
consolePage =
    do template "Administration" () $
         <div>
          <p>Enter a world of excitement.</p>
         </div>


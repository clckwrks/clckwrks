module Clckwrks.JS.Route where

import Control.Applicative     ((<$>))
import Clckwrks.Monad          (ClckT)
import Clckwrks.JS.ClckwrksApp (clckwrksAppJS)
import Clckwrks.JS.URL
import Happstack.Server        (Happstack, Response, ok, toResponse)
import Happstack.Server.JMacro ()

routeJS :: (Happstack m) => JSURL -> ClckT u m Response
routeJS url =
  case url of
    ClckwrksApp -> ok $ toResponse $ clckwrksAppJS

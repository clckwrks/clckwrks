module Clckwrks.Admin.Route where

import Clckwrks                    (AdminURL(..), Clck, ClckURL(..), Response, nestURL)
import Clckwrks.Admin.Console      (consolePage)
import Clckwrks.Admin.EditSettings (editSettings)
import Clckwrks.Menu.EditMenu      (editMenu, menuPost)

-- | routes for 'AdminURL'
routeAdmin :: AdminURL -> Clck ClckURL Response
routeAdmin url =
    case url of
      Console           -> nestURL Admin $ consolePage
      EditSettings      -> editSettings (Admin url)
      EditMenu          -> editMenu
      MenuPost          -> menuPost

module Clckwrks.Admin.Route where

import Clckwrks                    (AdminURL(..), Clck, ClckURL(..), Response, nestURL)
import Clckwrks.Admin.Console      (consolePage)
import Clckwrks.Admin.EditSettings (editSettings)
import Clckwrks.NavBar.EditNavBar  (editNavBar, navBarPost)
import Clckwrks.Admin.SystemEmails (systemEmailsPage)

-- | routes for 'AdminURL'
routeAdmin :: AdminURL -> Clck ClckURL Response
routeAdmin url =
    case url of
      Console           -> nestURL Admin $ consolePage
      EditSettings      -> editSettings (Admin url)
      EditNavBar        -> editNavBar
      NavBarPost        -> navBarPost
      SystemEmails      -> systemEmailsPage (Admin url)


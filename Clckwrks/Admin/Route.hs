module Clckwrks.Admin.Route where

import Clckwrks
import Clckwrks.Admin.Console
import Clckwrks.Admin.URL
--import Clckwrks.Admin.EditFeedConfig (editFeedConfig)
--import Clckwrks.Admin.EditPage       (editPage)
import Clckwrks.Admin.EditSettings   (editSettings)
-- import Clckwrks.Admin.NewPage        (newPage)
-- import Clckwrks.Admin.PreviewPage    (previewPage)
-- import Clckwrks.Admin.Pages
import Clckwrks.Menu.Acid
import Clckwrks.Menu.Edit
import Clckwrks.Menu.Types

-- | routes for 'AdminURL'
routeAdmin :: AdminURL -> Clck ClckURL Response
routeAdmin url =
    case url of
      Console           -> nestURL Admin $ consolePage
      EditSettings      -> editSettings   (Admin url)
      EditMenu          ->
          do menu <- query AskMenu
             editMenu (menu :: Menu ClckURL)
      MenuPOST          -> menuPost


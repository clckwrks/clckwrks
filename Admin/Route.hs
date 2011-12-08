module Admin.Route where

import Admin.Console 
import Admin.URL
import Admin.EditPage
import Admin.NewPage
import Clckwrks
import Menu.Acid
import Menu.Edit

routeAdmin :: AdminURL -> Clck ClckURL Response
routeAdmin url =
    case url of
      Console        -> nestURL Admin $ consolePage
      (EditPage pid) -> editPage (Admin url) pid
      NewPage        -> nestURL Admin $ newPage
      EditMenu       -> 
          do menu <- query AskMenu
             editMenu menu

{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Menu.API where

import Clckwrks.Menu.Types (Menu(..), MenuItem(..), MenuName(..), MenuLink(..))
import Clckwrks.Menu.Acid  (AskMenu(..))
import Clckwrks.Monad      (Clck, getPrefix, getUnique, query)
import Clckwrks.URL        (ClckURL)
import Data.Text           (Text)
import Data.Tree           (Forest, Tree(..))
import HSP                 hiding (escape)
import Web.Routes          (showURL)

mkMenuName :: Text -> Clck url MenuName
mkMenuName name =
    do p <- getPrefix
       u <- getUnique
       return $ MenuName { menuPrefix = p
                         , menuTag    = name
                         , menuUnique = u
                         }

getMenu :: GenXML (Clck ClckURL)
getMenu =
    do menu <- query AskMenu
       menuForestHTML $ menuItems menu

menuForestHTML :: Forest (MenuItem url) -> GenXML (Clck url)
menuForestHTML [] = return $ cdata ""
menuForestHTML forest =
    <ul class="page-menu">
     <% mapM menuTreeHTML forest %>
    </ul>

menuTreeHTML :: Tree (MenuItem url) -> GenXML (Clck url)
menuTreeHTML (Node menuItem subMenus) =
    case menuLink menuItem of
      (LinkURL url) ->
          do u <- showURL url
             <li>
              <a href=u><% menuTitle menuItem %></a>
              <% menuForestHTML subMenus %>
              </li>
      LinkMenu ->  -- FIXME: add real support for sub menus
          <li>sub-menu (fixme)</li>

             
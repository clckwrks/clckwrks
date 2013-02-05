{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Menu.API where

import Clckwrks
import Clckwrks.Menu.Acid
import Clckwrks.Menu.Types

getMenu :: GenXML (Clck ClckURL)
getMenu =
    do menu <- query GetMenu
       navBarHTML menu

navBarHTML :: Menu -> GenXML (Clck ClckURL)
navBarHTML (Menu menuItems) =
    <div class="navbar">
      <div class="navbar-inner">
        <ul class="nav">
          <% mapM mkMenuItem menuItems %>
        </ul>
      </div>
    </div>

mkMenuItem :: MenuItem -> GenXML (Clck ClckURL)
mkMenuItem (MILink (MenuLink ttl lnk)) =
    <li><a href=lnk><% ttl %></a></li>

{-
-- menuForestHTML :: Forest (MenuItem url) -> GenXML (Clck url)
menuForestHTML [] = return $ cdata ""
menuForestHTML forest =
    <ul class="page-menu">
     <% mapM menuTreeHTML forest %>
    </ul>

-- menuTreeHTML :: Tree (MenuItem url) -> GenXML (Clck url)
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
      LinkText txt ->
          <li>LinkText not implemented: <% txt %></li>


-}
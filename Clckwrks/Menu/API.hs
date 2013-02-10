{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Menu.API where

import Clckwrks
import Clckwrks.Menu.Acid
import Clckwrks.Menu.Types

getMenuData :: (Functor m, MonadIO m) => ClckT url m Menu
getMenuData = query GetMenu

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

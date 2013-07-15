{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.NavBar.API where

import Clckwrks
import Clckwrks.NavBar.Acid
import Clckwrks.NavBar.Types
import Data.Text.Lazy (Text)
import HSP.XMLGenerator
import HSP.XML

getNavBarData :: (Functor m, MonadIO m) => ClckT url m NavBar
getNavBarData = query GetNavBar

getNavBar :: GenXML (Clck ClckURL)
getNavBar =
    do navBar <- query GetNavBar
       navBarHTML navBar

navBarHTML :: NavBar -> GenXML (Clck ClckURL)
navBarHTML (NavBar navBarItems) =
    <div class="navbar">
      <div class="navbar-inner">
        <ul class="nav">
          <% mapM mkNavBarItem navBarItems %>
        </ul>
      </div>
    </div>

mkNavBarItem :: NavBarItem -> GenXML (Clck ClckURL)
mkNavBarItem (NBLink (NamedLink ttl lnk)) =
    <li><a href=lnk><% ttl %></a></li>

{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Menu.API where

import ClckwrksMonad
import Data.Text (Text)
import Data.Tree
import HSP hiding (escape)
import Menu.Types
import Menu.Acid
import Types
import URL

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
    <ol class="menu">
     <% mapM menuTreeHTML forest %>
    </ol>

menuTreeHTML :: Tree (MenuItem url) -> GenXML (Clck url)
menuTreeHTML (Node menuItem subMenus) =
    <li>
     <a><% menuTitle menuItem %></a>
     <% menuForestHTML subMenus %>
    </li>

             
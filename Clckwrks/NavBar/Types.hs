{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clckwrks.NavBar.Types where

import Clckwrks.Types (NamedLink(..))
import Data.Aeson     (ToJSON(..), (.=), object)
import Data.Data      (Data, Typeable)
import Data.SafeCopy  (base, deriveSafeCopy)
import Data.Text      (Text)

newtype NavBar = NavBar
    { navBarItems :: [NavBarItem]
    }
    deriving (Eq, Read, Show, Data, Typeable)



data NavBarItem
    = NBLink    NamedLink
    | NBSubNavBar Text NavBar
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''NavBar)
$(deriveSafeCopy 1 'base ''NavBarItem)

newtype NavBarLinks = NavBarLinks [(String, [NamedLink])]
    deriving (Eq, Read, Show, Data, Typeable)

instance ToJSON NavBarLinks where
    toJSON (NavBarLinks navBarLinks) =
        toJSON $ map (\(plugName, links) ->
                          object [ "pluginName" .= plugName
                                 , "pluginLinks" .= map toJSON links
                                 ]) navBarLinks


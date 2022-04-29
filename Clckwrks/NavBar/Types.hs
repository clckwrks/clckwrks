{-# LANGUAGE CPP, DeriveGeneric, DeriveDataTypeable, OverloadedStrings, RecordWildCards, TemplateHaskell, FlexibleContexts, UndecidableInstances #-}
module Clckwrks.NavBar.Types where

import Clckwrks.Types (NamedLink(..))
import Data.Aeson     (ToJSON(..), (.=), object)
import Data.Data      (Data, Typeable)
import Data.SafeCopy  (SafeCopy(..), base, contain, deriveSafeCopy, getSafePut, getSafeGet)
import Data.Serialize (label, putWord8, getWord8)
import Data.Text      (Text)
import GHC.Generics   (Generic)

newtype NavBar = NavBar
    { navBarItems :: [NavBarItem]
    }
    deriving (Eq, Read, Show, Data, Typeable, Generic)

data NavBarItem
    = NBLink    NamedLink
    | NBSubNavBar Text NavBar
    deriving (Eq, Read, Show, Data, Typeable, Generic)

#if __GLASGOW_HASKELL >= __900__
instance SafeCopy NavBar where version = 1
instance SafeCopy NavBarItem where version = 1
#else
$(deriveSafeCopy 1 'base ''NavBar)
$(deriveSafeCopy 1 'base ''NavBarItem)
#endif

newtype NavBarLinks = NavBarLinks [(String, [NamedLink])]
    deriving (Eq, Read, Show, Data, Typeable)

instance ToJSON NavBarLinks where
    toJSON (NavBarLinks navBarLinks) =
        toJSON $ map (\(plugName, links) ->
                          object [ "pluginName" .= plugName
                                 , "pluginLinks" .= map toJSON links
                                 ]) navBarLinks

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Clckwrks.Menu.Types where

import Data.Aeson    (ToJSON(..), (.=), object)
import Data.Data     (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text     (Text)

newtype Menu = Menu
    { menuItems :: [MenuItem]
    }
    deriving (Eq, Read, Show, Data, Typeable)

data MenuLink = MenuLink
    { menuItemName :: Text
    , menuItemLink :: Text
    }
    deriving (Eq, Read, Show, Data, Typeable)

newtype MenuLinks = MenuLinks [(String, [MenuLink])]
    deriving (Eq, Read, Show, Data, Typeable)

instance ToJSON MenuLinks where
    toJSON (MenuLinks menuLinks) =
        toJSON $ map (\(plugName, links) ->
                          object [ "pluginName" .= plugName
                                 , "pluginLinks" .= map toJSON links
                                 ]) menuLinks

instance ToJSON MenuLink where
    toJSON (MenuLink{..}) =
        object [ "menuItemName" .= menuItemName
               , "menuItemLink" .= menuItemLink
               ]

-- instance FromJSON MenuLink where
--    fromJSON (Object obj) =

data MenuItem
    = MILink    MenuLink
    | MISubMenu Text Menu
    deriving (Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''Menu)
$(deriveSafeCopy 1 'base ''MenuLink)
$(deriveSafeCopy 1 'base ''MenuItem)





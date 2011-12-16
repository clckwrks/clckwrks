{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Menu.Types where

-- import ClckwrksMonad (Clck, Prefix(..), getPrefix, getUnique)
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Aeson
import Data.Data
import Data.IxSet
import Data.SafeCopy
import Data.String
import Data.Text (Text)
import Data.Tree
import Data.Monoid
import Web.Routes
import Types

{-

It seems natural to store a menu+sub-menus as a Forest from Data.Tree. 

However, this turns out to be very limiting in practice. Data.Tree does not give us an easy way to lookup a specific sub-menu, or insert values, etc.

A zipper will not really help us either because of the disconnect of going between Haskell and HTML/browser and back.

We want to be able to:

- retrieve the entire menu structure
- retrieve a sub-menu
- re-order a menu or sub-menu or menu item
- insert a new menu or sub-menu or menu item
- a combination of static and dynamic menu generation

Can menu items be re-used between menus?
Can items exist that are not in a menu?
Should menu names be linked to the pages they come from?

Some parts of menus are generated in code.
Some parts are generated dynmaically.
Having ids in the code menus is tricky.

Code menus should probably not need to use ids.

We probably want a way of specifying how to merge menus from multiple plugins.

Not all menu items live in the state. But some do?

Who gets control over the ordering then? Do we have a Menu type. And then an order function?

are menus Monoids?

We can create a function, menuFromTree. That generates a menu from a datastructure in the code.

But, if we run that everytime, and it generates ids that are used for sorting stuff. Then how do we ensure the ids are the same? What happens if the menu in the code changes?

We have a problem that parts of the code are blindly inserting menu entries. We do not know where they go really, just that they need to exist. Other code has to handle the presentation of those menu entries. But it does not know all the menus in advanced. 

Everyone in the system is working with partial data.

The idea of assigning a weighted value to every item, and then sorting on weight the alphabet is attractive in its simpleness. But simple does not mean right. One problem is that a developer might assign some very high or very low weight to their menu items. Or, different libraries might assign different weights, but you want the order the other way.

In the end, only the user can decide what order they want the menu in. And they would like to be able to override everything. But, they don't want to have to do everything manually.

components from different vendors need ways to create unique ids. Guessing at globally unique ids is a sure plan for failure.

Seems that part of the component system should be a unique id generator. Part of that should be a unique prefix that the module can use. 

each component should be registered with a unique prefix. 

So the code generated menus could then use that to help name menu entries

-}

data MenuName = MenuName
    { menuPrefix :: Prefix
    , menuTag    :: Text
    , menuUnique :: Integer -- an integer which ensures this name is unique. However, it may not be stable across runs.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''MenuName)

data MenuLink url
    = LinkText Text
    | LinkURL url
    | LinkMenu -- (Menu url)
    deriving (Eq, Read, Show, Data, Typeable)

data MenuItem url = MenuItem 
    { menuName  :: MenuName
    , menuTitle :: Text
    , menuLink  :: MenuLink url
    }
    deriving (Eq, Read, Show, Data, Typeable)

data Menu url
    = Menu { menuItems :: Forest (MenuItem url)
           }
      deriving (Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''MenuLink)
$(deriveSafeCopy 1 'base ''MenuItem)
$(deriveSafeCopy 1 'base ''Menu)

instance Monoid (Menu url) where
    mempty = Menu { menuItems = [] }
    (Menu a) `mappend` (Menu b) = Menu $ a ++ b

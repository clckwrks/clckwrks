{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Menu.Acid
    where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Trans (liftIO)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable, IxSet, (@=), empty, fromList, getOne, ixSet, ixFun, insert, toList, updateIx)
import Data.SafeCopy
import Data.Text (Text)
import Data.Tree (Tree(..))
import qualified Data.Text as Text
import Menu.Types

data MenuState url  = MenuState 
    { menu      :: Menu url
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''MenuState)

initialMenuState :: MenuState url
initialMenuState = MenuState { menu = Menu [] }

askMenu :: Query (MenuState url) (Menu url)
askMenu =
    do MenuState{..} <- ask
       return menu

addItem :: MenuItem url -> Update (MenuState url) (Menu url)
addItem item =
    do ms@MenuState{..} <- get
       let menu' = Menu $ (menuItems menu) ++ [Node item []]
       put $ ms { menu = menu' }
       return menu'

setMenu :: Menu url -> Update (MenuState url) ()
setMenu newMenu =
    do ms <- get
       put $ ms { menu = newMenu }

$(makeAcidic ''MenuState ['askMenu, 'addItem, 'setMenu])

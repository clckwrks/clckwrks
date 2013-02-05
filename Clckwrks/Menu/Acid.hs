{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Menu.Acid where

import Clckwrks.Menu.Types  (Menu(..), MenuItem(..))
import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (modify)
import Data.Acid            (Update, Query, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.SafeCopy        (base, deriveSafeCopy)

data MenuState = MenuState
    { menu :: Menu
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'base ''MenuState)

initialMenuState :: MenuState
initialMenuState =
    MenuState { menu = Menu [] }

setMenu :: Menu
        -> Update MenuState ()
setMenu m = modify $ \ms -> ms { menu = m }

getMenu :: Query MenuState Menu
getMenu = menu <$> ask

$(makeAcidic ''MenuState ['getMenu, 'setMenu])

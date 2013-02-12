{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.NavBar.Acid where

import Clckwrks.NavBar.Types  (NavBar(..), NavBarItem(..))
import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (modify)
import Data.Acid            (Update, Query, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.SafeCopy        (base, deriveSafeCopy)

data NavBarState = NavBarState
    { navBar :: NavBar
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 2 'base ''NavBarState)

initialNavBarState :: NavBarState
initialNavBarState =
    NavBarState { navBar = NavBar [] }

setNavBar :: NavBar
        -> Update NavBarState ()
setNavBar m = modify $ \ms -> ms { navBar = m }

getNavBar :: Query NavBarState NavBar
getNavBar = navBar <$> ask

$(makeAcidic ''NavBarState ['getNavBar, 'setNavBar])

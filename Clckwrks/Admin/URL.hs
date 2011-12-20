{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.Admin.URL where

import Clckwrks.Page.Types (PageId(..))
import Data.Data           (Data, Typeable)
import Data.SafeCopy       (base, deriveSafeCopy)
import Web.Routes.TH       (derivePathInfo)

data AdminURL
    = Console
    | EditPage PageId
    | NewPage
    | EditMenu
    | MenuPOST
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''AdminURL)
$(deriveSafeCopy 1 'base ''AdminURL)
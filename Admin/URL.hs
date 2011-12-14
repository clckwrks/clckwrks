{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Admin.URL where

import Data.Data
import Data.SafeCopy
import Page.Types
import Web.Routes
import Web.Routes.TH

data AdminURL
    = Console
    | EditPage PageId
    | NewPage
    | EditMenu
    | MenuPOST
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''AdminURL)
$(deriveSafeCopy 1 'base ''AdminURL)
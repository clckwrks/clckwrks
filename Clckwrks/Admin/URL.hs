{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.Admin.URL where

import Data.Data           (Data, Typeable)
import Data.SafeCopy       (base, deriveSafeCopy)
import Web.Routes.TH       (derivePathInfo)

data AdminURL
    = Console
    | EditSettings
    | EditNavBar
    | NavBarPost
    | SystemEmails
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''AdminURL)
$(deriveSafeCopy 3 'base ''AdminURL)

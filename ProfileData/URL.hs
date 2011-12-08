{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module ProfileData.URL where

import Data.Data (Data, Typeable)
import Data.SafeCopy (SafeCopy(..), base, deriveSafeCopy)
import Web.Routes.TH

data ProfileDataURL
    = CreateNewProfileData
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)
$(deriveSafeCopy 1 'base ''ProfileDataURL)
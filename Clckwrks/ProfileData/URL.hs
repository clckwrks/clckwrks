{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.ProfileData.URL where

import Data.Data      (Data, Typeable)
import Data.SafeCopy  (SafeCopy(..), base, deriveSafeCopy)
import Happstack.Auth (UserId)
import Web.Routes.TH  (derivePathInfo)

data ProfileDataURL
    = CreateNewProfileData
    | EditProfileData
    | EditProfileDataFor UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)
$(deriveSafeCopy 1 'base ''ProfileDataURL)
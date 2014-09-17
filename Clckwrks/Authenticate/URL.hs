{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Authenticate.URL
       ( AuthURL(..)
       )
       where

import Data.Data                   (Data, Typeable)
import GHC.Generics                (Generic)
import Happstack.Authenticate.Core (AuthenticateURL(..))
import Web.Routes.TH               (derivePathInfo)

data AuthURL
  = Auth AuthenticateURL
  | Login
  deriving (Eq, Ord, Data, Typeable, Generic, Read, Show)

derivePathInfo ''AuthURL

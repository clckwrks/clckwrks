{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}
{- |URLs for JavaScript fragments
-}
module Clckwrks.JS.URL where

import Data.Data     (Data, Typeable)
import GHC.Generics  (Generic)
import Web.Routes.TH (derivePathInfo)

data JSURL
  = ClckwrksApp -- AngularJS App controller
  deriving (Eq, Ord, Data, Typeable, Generic, Read, Show)

derivePathInfo ''JSURL

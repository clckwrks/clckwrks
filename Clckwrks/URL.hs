{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeFamilies #-}
module Clckwrks.URL
     ( ClckURL(..)
     , AdminURL(..)
     , AuthenticateURL(..)
     , NoEscape(..)
     ) where

import Clckwrks.Admin.URL          (AdminURL(..))
import Clckwrks.JS.URL             (JSURL)
import Clckwrks.ProfileData.URL    (ProfileDataURL(..))
import Control.Applicative         ((<$>), many)
import Data.Data                   (Data, Typeable)
import Data.SafeCopy               (Migrate(..), SafeCopy(..), base, deriveSafeCopy, extension)
import Data.Text                   (Text, pack, unpack)
import Happstack.Authenticate.Core (AuthenticateURL(..))
import System.FilePath             (joinPath, splitDirectories)
import Web.Routes                  (PathInfo(..), anySegment)
import Web.Routes.TH               (derivePathInfo)

newtype NoEscape a = NoEscape a
      deriving (Eq, Ord, Data, Typeable, Read, Show)

instance PathInfo (NoEscape String) where
    toPathSegments (NoEscape s) = map pack $ splitDirectories s
    fromPathSegments =
        do ps <- many anySegment
           return (NoEscape (joinPath $ map unpack ps))

data ClckURL
    = ThemeData String
    | ThemeDataNoEscape (NoEscape FilePath)
    | PluginData Text FilePath
    | Admin AdminURL
    | Profile ProfileDataURL
    | JS JSURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)

$(derivePathInfo ''ClckURL)

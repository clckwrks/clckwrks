{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeFamilies #-}
module Clckwrks.URL
     ( ClckURL(..)
     , AdminURL(..)
     , AuthURL(..)
     , ProfileURL(..)
     , AuthProfileURL(..)
     , ProfileDataURL(..)
     , NoEscape(..)
     ) where

import Clckwrks.Admin.URL          (AdminURL(..))
-- import Clckwrks.Page.Acid          (PageId(..))
-- import Clckwrks.Page.Types         (Slug(..))
import Clckwrks.ProfileData.URL    (ProfileDataURL(..))
import Control.Applicative         ((<$>), many)
import Data.Data                   (Data, Typeable)
import Data.SafeCopy               (Migrate(..), SafeCopy(..), base, deriveSafeCopy, extension)
import Data.Text                   (Text, pack, unpack)
import Happstack.Auth              (AuthURL(..), ProfileURL(..), AuthProfileURL(..), UserId)
import Happstack.Auth.Core.AuthURL (OpenIdURL, AuthMode, OpenIdProvider)
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
    | Auth AuthProfileURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)

-- TODO: move upstream
$(deriveSafeCopy 1 'base ''AuthURL)
$(deriveSafeCopy 1 'base ''ProfileURL)
$(deriveSafeCopy 1 'base ''AuthProfileURL)
$(deriveSafeCopy 1 'base ''OpenIdURL)
$(deriveSafeCopy 1 'base ''AuthMode)
$(deriveSafeCopy 1 'base ''OpenIdProvider)

$(derivePathInfo ''ClckURL)
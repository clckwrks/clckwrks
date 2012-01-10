{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.URL 
     ( ClckURL(..)
     , AdminURL(..)
     , AuthURL(..)
     , ProfileURL(..)
     , AuthProfileURL(..)
     , ProfileDataURL(..)
     ) where

import Clckwrks.Admin.URL          (AdminURL(..))
import Clckwrks.Page.Acid          (PageId(..))
import Clckwrks.ProfileData.URL    (ProfileDataURL(..))
import Control.Applicative         ((<$>))
import Data.Data                   (Data, Typeable)
import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Data.Text                   (Text)
import Happstack.Auth              (AuthURL(..), ProfileURL(..), AuthProfileURL(..))
import Happstack.Auth.Core.AuthURL (OpenIdURL, AuthMode, OpenIdProvider)

import Web.Routes.TH               (derivePathInfo)

data ClckURL
    = ViewPage PageId
    | Blog
    | ThemeData FilePath
    | PluginData Text FilePath
    | Admin AdminURL
    | Profile ProfileDataURL
    | Auth AuthProfileURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)

-- TODO: move upstream
$(deriveSafeCopy 1 'base ''AuthURL)
$(deriveSafeCopy 1 'base ''ProfileURL)
$(deriveSafeCopy 1 'base ''AuthProfileURL)
$(deriveSafeCopy 1 'base ''ClckURL)
$(deriveSafeCopy 1 'base ''OpenIdURL)
$(deriveSafeCopy 1 'base ''AuthMode)
$(deriveSafeCopy 1 'base ''OpenIdProvider)

$(derivePathInfo ''ClckURL)
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
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
import Clckwrks.Page.Types         (Slug(..))
import Control.Applicative         ((<$>))
import Data.Data                   (Data, Typeable)
import Data.SafeCopy               (Migrate(..), SafeCopy(..), base, deriveSafeCopy, extension)
import Data.Text                   (Text)
import Happstack.Auth              (AuthURL(..), ProfileURL(..), AuthProfileURL(..), UserId)
import Happstack.Auth.Core.AuthURL (OpenIdURL, AuthMode, OpenIdProvider)
import Web.Routes.TH               (derivePathInfo)

data ClckURL_1
    = ViewPage_1 PageId
    | Blog_1
    | AtomFeed_1
    | ThemeData_1 FilePath
    | PluginData_1 Text FilePath
    | Admin_1 AdminURL
    | Profile_1 ProfileDataURL
    | Auth_1 AuthProfileURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 1 'base ''ClckURL_1)

data ClckURL
    = ViewPage PageId
    | ViewPageSlug PageId Slug
    | Blog
    | AtomFeed
    | ThemeData FilePath
    | PluginData Text FilePath
    | Admin AdminURL
    | Profile ProfileDataURL
    | Auth AuthProfileURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 2 'extension ''ClckURL)

instance Migrate ClckURL where
    type MigrateFrom ClckURL   = ClckURL_1
    migrate (ViewPage_1 pid)   = ViewPage pid
    migrate Blog_1             = Blog
    migrate AtomFeed_1         = AtomFeed
    migrate (ThemeData_1 fp)   = ThemeData fp
    migrate (PluginData_1 t f) = PluginData t f
    migrate (Admin_1 u)        = Admin u
    migrate (Profile_1 pdu)    = Profile pdu
    migrate (Auth_1 apu)       = Auth apu

-- TODO: move upstream
$(deriveSafeCopy 1 'base ''AuthURL)
$(deriveSafeCopy 1 'base ''ProfileURL)
$(deriveSafeCopy 1 'base ''AuthProfileURL)
$(deriveSafeCopy 1 'base ''OpenIdURL)
$(deriveSafeCopy 1 'base ''AuthMode)
$(deriveSafeCopy 1 'base ''OpenIdProvider)


$(derivePathInfo ''ClckURL)
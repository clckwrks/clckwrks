{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Clckwrks.Types
    ( UUID
    , Prefix(..)
    , Trust(..)
    , NamedLink(..)
    ) where

import Data.Aeson    (ToJSON(..), (.=), object)
import Data.Data     (Data, Typeable)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text)
import Data.UUID     (UUID)
import HSP.Google.Analytics (UACCT)


-- | 'SafeCopy' instances for some 3rd party types
$(deriveSafeCopy 0 'base ''UACCT)
$(deriveSafeCopy 0 'base ''UUID)

-- | at present this is only used by the menu editor
newtype Prefix = Prefix { prefixText :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data Trust
    = Trusted   -- ^ used when the author can be trusted     (sanitization is not performed)
    | Untrusted -- ^ used when the author can not be trusted (sanitization is performed)
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Trust)

data NamedLink = NamedLink
    { namedLinkTitle :: Text
    , namedLinkURL   :: Text
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''NamedLink)

instance ToJSON NamedLink where
    toJSON (NamedLink{..}) =
        object [ "navBarItemName" .= namedLinkTitle
               , "navBarItemLink" .= namedLinkURL
               ]


{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.Types where

import Data.Data     (Data, Typeable)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     as T

-- | at present this is only used by the menu editor
newtype Prefix = Prefix { prefixText :: T.Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data Trust
    = Trusted    -- ^ used when the author can be trusted     (sanitization is not performed)
    | Untrusted -- ^ used when the author can not be trusted (sanitization is performed)
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Trust)
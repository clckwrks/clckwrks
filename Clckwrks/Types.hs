{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Clckwrks.Types where

import Data.Data     (Data, Typeable)
import Data.SafeCopy (SafeCopy)
import Data.Text     as T

newtype Prefix = Prefix { prefixText :: T.Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

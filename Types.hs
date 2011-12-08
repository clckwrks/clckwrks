{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Types where

import Data.Data
import Data.SafeCopy
import Data.Text as T

newtype Prefix = Prefix { prefixText :: T.Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

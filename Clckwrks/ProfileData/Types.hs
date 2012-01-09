{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.ProfileData.Types 
     ( ProfileData(..)
     , Role(..)
     , emptyProfileData
     ) where

import Happstack.Auth (UserId(..))
import Data.Data     (Data, Typeable)
import Data.IxSet    (Indexable(..), ixSet, ixFun)
import Data.Map      (Map, empty)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set      (Set, empty)
import Data.Text     (Text, empty)

data Role 
    = Administrator
    | Visitor
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''Role)

data ProfileData = ProfileData
    { dataFor    :: UserId
    , username   :: Text
    , email      :: Maybe Text
    , roles      :: Set Role
    , attributes :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''ProfileData)

emptyProfileData :: ProfileData
emptyProfileData = ProfileData
   { dataFor    = UserId 0
   , username   = Data.Text.empty
   , email      = Nothing
   , roles      = Data.Set.empty
   , attributes = Data.Map.empty
   }

instance Indexable ProfileData where
    empty = ixSet [ ixFun $ (:[]) . dataFor ]

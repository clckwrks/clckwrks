{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.ProfileData.Types
     ( ProfileData(..)
     , Role(..)
     , defaultProfileDataFor
     , emptyProfileData
     , Username(..)
     ) where

import Data.Data     (Data, Typeable)
import Data.IxSet    (Indexable(..), ixSet, ixFun)
import Data.IxSet.Ix (Ix)
import Data.Map      (Map, empty)
import Data.SafeCopy (Migrate(..), base, deriveSafeCopy, extension)
import Data.Set      (Set, empty, singleton)
import Data.Text     (Text, empty)
import Data.Typeable (Typeable)
import Data.UserId   (UserId(..))

data Role_001
    = Administrator_001
    | Visitor_001
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded)
$(deriveSafeCopy 1 'base ''Role_001)

data Role
    = Administrator
    | Visitor
    | Moderator
    | Editor
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded)
$(deriveSafeCopy 2 'extension ''Role)

instance Migrate Role where
    type MigrateFrom Role = Role_001
    migrate Administrator_001 = Administrator
    migrate Visitor_001       = Visitor


data ProfileData_1 = ProfileData_1
    { dataFor_1    :: UserId
    , username_1   :: Text       -- ^ now comes from happstack-authenticate
    , email_1      :: Maybe Text -- ^ now comes from happstack-authenticate
    , roles_1      :: Set Role
    , attributes_1 :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''ProfileData_1)

data ProfileData = ProfileData
    { dataFor    :: UserId
    , roles      :: Set Role
    , attributes :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 2 'extension ''ProfileData)

instance Migrate ProfileData where
  type MigrateFrom ProfileData = ProfileData_1
  migrate (ProfileData_1 df _ _ rs attrs) = ProfileData df rs attrs


emptyProfileData :: ProfileData
emptyProfileData = ProfileData
   { dataFor    = UserId 0
   , roles      = Data.Set.empty
   , attributes = Data.Map.empty
   }

defaultProfileDataFor :: UserId -> ProfileData
defaultProfileDataFor uid =
  emptyProfileData { dataFor = uid
                   , roles   = singleton Visitor
                   }

newtype Username = Username { unUsername :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Indexable ProfileData where
    empty = ixSet [ ixFunS dataFor
                  ]
        where
          ixFunS :: (Ord b, Typeable b) => (a -> b) -> Ix a
          ixFunS f = ixFun $ \a -> [f a]

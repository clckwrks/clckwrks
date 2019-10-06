{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, TypeFamilies #-}
module Clckwrks.ProfileData.Types
     ( DisplayName(..)
     , ProfileData(..)
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
import GHC.Generics  (Generic)

data Role_001
    = Administrator_001
    | Visitor_001
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded, Generic)
$(deriveSafeCopy 1 'base ''Role_001)

data Role
    = Administrator
    | Visitor
    | Moderator
    | Editor
      deriving (Eq, Ord, Read, Show, Data, Typeable, Enum, Bounded, Generic)
$(deriveSafeCopy 2 'extension ''Role)

instance Migrate Role where
    type MigrateFrom Role = Role_001
    migrate Administrator_001 = Administrator
    migrate Visitor_001       = Visitor

newtype Username = Username { unUsername :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

newtype DisplayName = DisplayName { unDisplayName :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)
$(deriveSafeCopy 1 'base ''DisplayName)

data ProfileData_1 = ProfileData_1
    { dataFor_1    :: UserId
    , username_1   :: Text       -- ^ now comes from happstack-authenticate
    , email_1      :: Maybe Text -- ^ now comes from happstack-authenticate
    , roles_1      :: Set Role
    , attributes_1 :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

$(deriveSafeCopy 1 'base ''ProfileData_1)

data ProfileData_2 = ProfileData_2
    { dataFor_2    :: UserId
    , roles_2      :: Set Role
    , attributes_2 :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

$(deriveSafeCopy 2 'extension ''ProfileData_2)

instance Migrate ProfileData_2 where
  type MigrateFrom ProfileData_2 = ProfileData_1
  migrate (ProfileData_1 df _ _ rs attrs) = ProfileData_2 df rs attrs

data ProfileData = ProfileData
    { dataFor     :: UserId
    , displayName :: Maybe DisplayName
    , roles       :: Set Role
    , attributes  :: Map Text Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

$(deriveSafeCopy 3 'extension ''ProfileData)

instance Migrate ProfileData where
  type MigrateFrom ProfileData = ProfileData_2
  migrate (ProfileData_2 df rs attrs) = ProfileData df Nothing rs attrs

emptyProfileData :: ProfileData
emptyProfileData = ProfileData
   { dataFor     = UserId 0
   , displayName = Nothing
   , roles       = Data.Set.empty
   , attributes  = Data.Map.empty
   }

defaultProfileDataFor :: UserId -> ProfileData
defaultProfileDataFor uid =
  emptyProfileData { dataFor = uid
                   , roles   = singleton Visitor
                   }

instance Indexable ProfileData where
    empty = ixSet [ ixFunS dataFor
                  ]
        where
          ixFunS :: (Ord b, Typeable b) => (a -> b) -> Ix a
          ixFunS f = ixFun $ \a -> [f a]

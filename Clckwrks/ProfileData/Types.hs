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

defaultProfileDataFor :: UserId -> ProfileData
defaultProfileDataFor uid =
  emptyProfileData { dataFor = uid
                   , roles   = singleton Visitor
                   }

newtype Username = Username { unUsername :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Indexable ProfileData where
    empty = ixSet [ ixFunS dataFor
                  , ixFunS $ Username . username
                  ]
        where
          ixFunS :: (Ord b, Typeable b) => (a -> b) -> Ix a
          ixFunS f = ixFun $ \a -> [f a]

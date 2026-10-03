{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Rebac.Types where

import AccessControl.Check    (Access(..))
import AccessControl.Schema   (KnownPermission, Permission(..), ToPermission(..))
import AccessControl.Relation (ToObject(..), Object(..), ObjectId(..), ObjectType(..),Relation(..), ToRelation(..) )
import Data.Data              (Data, Typeable)
import Data.SafeCopy          (SafeCopy(..), base, contain, safeGet, safePut)
import Data.Text              (Text)
import GHC.Generics           (Generic)
import Web.Routes.TH          (derivePathInfo)

data RebacRelation
  = RebacAdmin
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToRelation RebacRelation where
  toRelation r =
    Relation $ case r of
                 RebacAdmin -> "admin"

data RebacPermission
  = RebacView
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToPermission RebacPermission where
  toPermission p =
    Permission $ case p of
                   RebacView -> "rebac_view"

newtype SchemaText = SchemaText { unSchemaText :: Text }
  deriving (Eq, Ord, Data, Read, Show, Generic)

instance SafeCopy SchemaText where version = 1 ; kind = base

newtype SchemaId = SchemaId { unSchemaId :: Int }
  deriving (Eq, Ord, Data, Read, Show, Generic, Enum)

derivePathInfo ''SchemaId

instance SafeCopy SchemaId where version = 1 ; kind = base

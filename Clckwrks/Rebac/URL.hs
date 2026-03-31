{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Rebac.URL
       ( RebacURL(..)
       )
       where

-- import Clckwrks.AccessControl      () -- ToObject UserId
import AccessControl.Relation      (ToObject(..), Object(..), ObjectType(..), ObjectId(..))
import AccessControl.Schema        (KnownPermission(..))
import Clckwrks.Rebac.Types        (RebacPermission(..), SchemaId)
import Clckwrks.Types              ()
import Data.Data                   (Data, Typeable)
import Data.UserId                 (UserId(..))
import GHC.Generics                (Generic)
import Web.Routes.TH               (derivePathInfo)

data RebacURL
  = SchemaPanel
  | ViewSchema SchemaId
  | RelationsPanel
  | RelationLogPanel
  deriving (Eq, Ord, Data, Typeable, Generic, Read, Show)

derivePathInfo ''RebacURL

instance ToObject RebacURL where
  toObject u =
    let objId = ObjectId $ case u of
          SchemaPanel    -> "schema_panel"
          RelationsPanel -> "relations_panel"
          RelationLogPanel -> "relation_log_panel"
          (ViewSchema _)   -> "view_schema"
    in Object (ObjectType "rebac_url") objId

instance KnownPermission RebacURL RebacPermission (Maybe UserId)


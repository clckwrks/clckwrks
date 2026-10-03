{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}
module Clckwrks.Rebac.API
       ( checkSchema
       , getRelationTuples
       , getRelationLog
       , getSchema
       , addRelationTuple
       , removeRelationTuple
       , updateSchema
       , RebacApi(..)
       , direct_member
       , usergroup
       ) where

import AccessControl.Check          (Access(..))
import AccessControl.Relation       (Relation(..), RelationTuple(..), ToObject(..), Object(..), ObjectId(..), ToRelation(..), ObjectType(..))
import AccessControl.Schema         (KnownPermission, Permission(..), Schema(..), ToPermission(..), parseSchema)
import AccessControl.Validate       (Valid(..), validate)
import Clckwrks.AccessControl       (checkAccess)
import Clckwrks.Authenticate.Plugin (authenticatePlugin, authenticatePluginLoader)
import Clckwrks.Authenticate.Monad  (AuthenticatePluginState(..))
import Clckwrks.Monad               (Clck, ClckPlugins, ClckT, plugins, query, update)
import Clckwrks.Rebac.Acid          (AddRelationTuple(..), AddRelationTupleError(..), CheckSchema(..), GetRelationLog(..), GetRelationTuples(..), GetSchema(..), RemoveRelationTuple(..), RelationLogEntry, UpdateSchema(..), UpdateSchemaError(..), UpdateSchemaResult(..))
import Clckwrks.Rebac.Types         (SchemaText(..))
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (modifyTVar')
import Control.Monad                (join)
import Control.Monad.State          (get)
import Control.Monad.Trans          (MonadIO(liftIO))
-- import Data.Acid as Acid            (AcidState, query, update)
import Data.Data                    (Data)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import Data.Maybe                   (maybe)
import Data.Monoid                  (mempty)
import Data.Text                    (Text)
import qualified Data.Text          as Text
import Data.Time.Clock              (getCurrentTime)
import Data.Time.Clock.POSIX        (POSIXTime, getPOSIXTime)
import Data.Typeable                (Typeable)
import Data.UserId                  (UserId)
import GHC.Generics                 (Generic)
import Happstack.Server             (Happstack)
import Web.Plugins.Core             (Plugin(..), When(Always), addCleanup, addHandler, addPluginState, getConfig, getPluginRouteFn, getPluginState, getPluginsSt, initPlugin, modifyPluginState')

data RebacApi
  = SchemaR
  | RelationsR
  | RelationLogR
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance ToObject RebacApi where
  toObject SchemaR =
    Object (ObjectType "rebac_api") (ObjectId "schema")
  toObject RelationsR =
    Object (ObjectType "rebac_api") (ObjectId "relations")
  toObject RelationLogR =
    Object (ObjectType "rebac_api") (ObjectId "relation_log")

instance KnownPermission RebacApi Permission (Maybe UserId)

-- | get the rebac schema from 'ClckState'
--
-- Will fail if the 'UserId' making the request does not have get permissions on 'SchemaR'
getSchema :: (Happstack m, MonadIO m) => ClckT url m (Either Text SchemaText)
getSchema =
  do now <- liftIO getPOSIXTime
     a <- checkAccess SchemaR (Permission "get") (Just now)
     case a of
       Allowed ->
         do s <- query GetSchema
            pure $ Right $ s
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons

checkSchema :: (MonadIO m) => SchemaText -> Text -> ClckT url m (Maybe UpdateSchemaError)
checkSchema s comment =
  do now <- liftIO getCurrentTime
     query (CheckSchema s)

updateSchema :: (MonadIO m) => SchemaText -> Text -> ClckT url m UpdateSchemaResult
updateSchema s comment =
  do now <- liftIO getCurrentTime
     update (UpdateSchema now s comment)

{-
updateSchemaText :: (MonadIO m) => Text -> ClckT url m UpdateSchemaResult
updateSchemaText txt =
  case parseSchema txt of
    Left e    -> pure (SchemaUpdateFailed (SchemaParseError e))
    Right sch -> update (UpdateSchema sch)

-}
-- | get all the relation tuples from the database.
--
-- Will fail if the 'UserId' making the request does not have 'get' permissions on 'RelationsR'
getRelationTuples :: (Happstack m, MonadIO m) => ClckT url m (Either Text [ RelationTuple ])
getRelationTuples =
  do now <- liftIO getPOSIXTime
     a <- checkAccess RelationsR (Permission "get") (Just now)
     case a of
       Allowed ->
         do ts <- query GetRelationTuples
            pure $ Right $ ts
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons

-- | get all the relation tuple log history from the database.
--
-- Will fail if the 'UserId' making the request does not have 'get' permissions on 'RelationLogR'
getRelationLog :: (Happstack m, MonadIO m) => ClckT url m (Either Text [ RelationLogEntry ])
getRelationLog =
  do now <- liftIO getPOSIXTime
     a <- checkAccess RelationLogR (Permission "get") (Just now)
     case a of
       Allowed ->
         do rl <- query GetRelationLog
            pure $ Right $ rl
       NotAllowed reasons ->
         pure $ Left $ Text.pack $ show reasons


-- | Add a 'RelationTuple' to the relation database.
--
-- NOTE: no checks are performed to ensure that the current user has
-- permission to modify the database.
addRelationTuple :: (Happstack m, MonadIO m) => RelationTuple -> Text -> ClckT url m (Either AddRelationTupleError RelationLogEntry)
addRelationTuple rt comment =
  do now <- liftIO getCurrentTime
     rle <- update (AddRelationTuple rt now comment)
     pure rle
{-
-- | Add a bunch of 'RelationTuple' to the relation database.
--
-- NOTE: no checks are performed to ensure that the current user has
-- permission to modify the database.
addRelationTuples :: (Happstack m, MonadIO m) => RelationTuple -> Text -> ClckT url m RelationLogEntry
addRelationTuples rt comment =
  do now <- liftIO getCurrentTime
     update (AddRelationTuple rt now comment)
-}
-- | Remove a 'RelationTuple' from the relation database.
--
-- NOTE: no checks are performed to ensure that the current user has
-- permission to modify the database.
removeRelationTuple :: (Happstack m, MonadIO m) => RelationTuple -> Text -> ClckT url m RelationLogEntry
removeRelationTuple rt comment =
  do now <- liftIO getCurrentTime
     update (RemoveRelationTuple rt now comment)

-- * core clckwrks Relations and ObjectTypes

direct_member :: Relation
direct_member = Relation "direct_member"

usergroup :: ObjectType
usergroup = ObjectType "usergroup"

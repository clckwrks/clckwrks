{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Clckwrks.Rebac.Acid where

import AccessControl.Check         (Access, checkIndexed)
import qualified AccessControl.Check as Check
import AccessControl.Relation      (Object, ObjectType, ObjectWildcard(NoWildcard), RelationTuple(..), Tag(..), hasTag, rels)
import AccessControl.Schema        (Permission, Schema(definitions), SchemaError, parseSchema, ppSchemaError, validateSchema)
import AccessControl.TupleIndex    (TupleIndex, deleteTuple, elemTuple, fromTuples, insertTuple, toTuples)
import AccessControl.Validate      (RelPerm(..), Valid(..), ValidationError(..), isValid, mkDefMap, ppValidationError, validate)
import Clckwrks.Rebac.Types        (SchemaId(..), SchemaText(..))
import Control.Monad.Reader        (ask)
import Control.Monad.State         (put, get)
import Data.Acid                   (Query, Update, makeAcidic)
import Data.Data                   (Data)
import Data.List                   (mapAccumL, nub, partition)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import Data.SafeCopy               (Migrate(..), SafeCopy(..), base, contain, extension, safeGet, safePut)
import AccessControl.SchemaFragment (SchemaFragment(fragmentText), schemaFragment)
import Data.Time.Clock             (UTCTime)
import Data.Time.Clock.POSIX       (POSIXTime, getCurrentTime)
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

newtype RelationTxId = RelationTxId { unRelationTxId :: Integer }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RelationTxId where version = 1 ; kind = base

succRelationIxId :: RelationTxId -> RelationTxId
succRelationIxId (RelationTxId rid) = (RelationTxId (succ rid))

ppRelationTxId :: RelationTxId -> Doc
ppRelationTxId (RelationTxId n) = PP.text "RelationTxId" <+> PP.text (show n)

data RLEAction
  = RLEAdd
  | RLERemove
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RLEAction where version = 1 ; kind = base

{-
It may seem like the Timestamp and Transaction Id are nearly duplicates. But fast transactions could result in identical time stamps and there are sitautions where the clock could appear to jump back in time.

-}
data RelationLogEntry
  = RelationLogEntry
    { rleTimestamp     :: UTCTime
    , rleRelationTuple :: RelationTuple
    , rleAction        :: RLEAction
    , rleComment       :: Text
    , rleTxId          :: RelationTxId
    }
  | SchemaUpdate
    { rleSchemaUpdateTimestamp :: UTCTime
    , rleSchemaId              :: SchemaId
    , rleComment               :: Text
    , rleSchemaFromTxId        :: RelationTxId -- ^ this schema applies to relation log entries whose 'rleTxId' is >= this
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | 2026-10-29: version 2 adds 'rleSchemaFromTxId'.
--
-- There is deliberately no 'Migrate' instance from 'RelationLogEntry_v1'. The
-- correct 'rleSchemaFromTxId' depends on where the entry sits in 'rsLog', which a
-- per-entry migration cannot see. The conversion is done by the 'Migrate'
-- instance for 'RebacState' instead, and anything else that tries to read a v1
-- entry fails loudly rather than getting a made-up value.
instance SafeCopy RelationLogEntry where version = 2 ; kind = base

data RelationLogEntry_v1
  = RelationLogEntry_v1
    { rleTimestamp_v1     :: UTCTime
    , rleRelationTuple_v1 :: RelationTuple
    , rleAction_v1        :: RLEAction
    , rleComment_v1       :: Text
    , rleTxId_v1          :: RelationTxId
    }
  | SchemaUpdate_v1
    { rleSchemaUpdateTimestamp_v1 :: UTCTime
    , rleSchemaId_v1              :: SchemaId
    , rleComment_v1               :: Text
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy RelationLogEntry_v1 where version = 1 ; kind = base

-- | the 'SchemaId' that was active when relation transaction 'txId' was performed
--
-- When several schema updates share a 'rleSchemaFromTxId' (no relation changes
-- happened between them) the most recent one, the highest 'SchemaId', wins.
schemaIdForTxId :: RelationTxId -> [RelationLogEntry] -> Maybe SchemaId
schemaIdForTxId txId rles =
  case [ (fromTxId, sid) | SchemaUpdate { rleSchemaId = sid, rleSchemaFromTxId = fromTxId } <- rles, fromTxId <= txId ] of
    [] -> Nothing
    xs -> Just (snd (maximum xs))

{-

Tuple logging:

 - tuples can be added and removed
 - does it make sense for a tuple to be modified? A user might originally only have read access to a resource but then be upgrade to read+write. Or perhaps the way an object id is encoded changes and there is a migration?
 - with only add/remove the history chain will be boring. A tuple is added, possibly removed, and perhaps added again/removed more times.
 - let's assume that modifications are going to be important since they have better potential to retain a history

 - the owner of an object (such as a document) wants to be able to view the history all the permissions for that document. Even if some subject can no longer access the object, knowing they could for a while could be important information.

 - one view we might want to see is a purely chronologic view of all relation tuples
 - one view we want to see is the chronologic history of a specific resource
 - one view we want to see is the chonological history of a specific resource and subject

 - most of the time we only want to access the currently active relation tuples

Should a relation tuple have unique id?
Should each operation have a unique, sequential id?

The current rebac backend wants things in a nice simple form -- would it actually be slower if it just looked directly at a the log?

The most common operation -- `check` starts by looking up a Resource.

If we store the log and the current state separately, what happens if they get out of sync?

We don't want to replay the whole log at startup -- but maybe we don't need to? The head of each resource will be the current state -- so we just slurp out that info.

storing the log separately could mean that it could be stored on disk or other slower storage media if it grows too large without affect the speed of the check operation.

It is not clear that modifying a relation is a sensible operation.

Let's stick with read/write/delete.

The `rsDefMap` is derived from the Schema. It seems a bit silly to
have both the Schema and the DefMap in the acid-state. But we do
actually need use the data in both forms. We could modify the
`SafeCopy` instance so that when the data is serialized, the DefMap
make is not actually stored, and is instead recalculated during
deserialization. But that seems like premature optimization.

-}

data RebacState = RebacState
  { rsSchema        :: SchemaText           -- currently active schema
  , rsSchemaHistory :: Map SchemaId SchemaText
  , rsDefMap        :: Map Text RelPerm     -- derived from Schema.
  , rsTuples        :: TupleIndex           -- currently important tuples, indexed; stored as the plain list it was in v2
  , rsLog           :: [ RelationLogEntry ] -- how did we get here
  , rsTxId          :: RelationTxId         -- next unassigned TxId
  , rsSchemaId      :: SchemaId             -- next unassigned SchemaId
  }
  deriving (Eq, Ord, Read, Show, Generic)

-- | 2026-10-29: version 2, the 'rsLog' entries gained 'rleSchemaFromTxId'
instance SafeCopy RebacState where version = 2 ; kind = extension

instance Migrate RebacState where
  type MigrateFrom RebacState = RebacState_v1
  migrate rs =
    RebacState
      { rsSchema        = rsSchema_v1 rs
      , rsSchemaHistory = rsSchemaHistory_v1 rs
      , rsDefMap        = rsDefMap_v1 rs
      , rsTuples        = fromTuples (rsTuples_v1 rs)
      , rsLog           = migrateRelationLog_v1 (rsLog_v1 rs)
      , rsTxId          = rsTxId_v1 rs
      , rsSchemaId      = rsSchemaId_v1 rs
      }

-- | fill in 'rleSchemaFromTxId', which v1 did not record
--
-- A v1 'SchemaUpdate' is only placed by its position in 'rsLog', which is
-- newest first. Walk it oldest first: a schema update applies from the
-- transaction after the last relation entry that preceded it, or from 0 if
-- none did (the initial schema precedes the initial tuples, which have
-- 'RelationTxId' 0).
--
-- 'removeRelationTuplesByTag' can consume a 'RelationTxId' without logging
-- anything, so the value computed here may be lower than the 'rsTxId' was at
-- the time. It still separates the entries before the update from the ones
-- after it, which is all 'schemaIdForTxId' relies on.
migrateRelationLog_v1 :: [RelationLogEntry_v1] -> [RelationLogEntry]
migrateRelationLog_v1 = reverse . snd . mapAccumL step (RelationTxId 0) . reverse
  where
    step next (RelationLogEntry_v1 ts rt action comment txId) =
      (max next (succRelationIxId txId), RelationLogEntry ts rt action comment txId)
    step next (SchemaUpdate_v1 ts sid comment) =
      (next, SchemaUpdate ts sid comment next)

data RebacState_v1 = RebacState_v1
  { rsSchema_v1        :: SchemaText
  , rsSchemaHistory_v1 :: Map SchemaId SchemaText
  , rsDefMap_v1        :: Map Text RelPerm
  , rsTuples_v1        :: [ RelationTuple ]
  , rsLog_v1           :: [ RelationLogEntry_v1 ]
  , rsTxId_v1          :: RelationTxId
  , rsSchemaId_v1      :: SchemaId
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy RebacState_v1 where version = 1 ; kind = base

getSchema :: Query RebacState SchemaText
getSchema = rsSchema <$> ask

getSchemaById :: SchemaId -> Query RebacState (Maybe SchemaText)
getSchemaById schemaId =
  do sh <- rsSchemaHistory <$> ask
     pure $ Map.lookup schemaId sh

-- | Only ever returned from an event or used in memory, never stored in
-- 'RebacState', so adding a constructor does not need a migration.
data UpdateSchemaError
  = SchemaDoesNotAllowRelations [ValidationError]
  | SchemaParseError String
  | SchemaInconsistent [SchemaError] -- ^ the schema is not consistent with itself, see 'validateSchema'
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy UpdateSchemaError where version = 1 ; kind = base

ppUpdateSchemaError :: UpdateSchemaError -> Doc
ppUpdateSchemaError (SchemaDoesNotAllowRelations vErrors) = PP.text "Schema does not allow the following relations. " $+$ (PP.nest 2 (PP.vcat $ map ppValidationError vErrors))
ppUpdateSchemaError (SchemaParseError errStr) = (PP.text "Schema parse error") <+> (PP.vcat $ map PP.text (lines errStr))
ppUpdateSchemaError (SchemaInconsistent errs) = PP.text "Schema is not consistent." $+$ (PP.nest 2 (PP.vcat $ map ppSchemaError errs))

-- | Parse a new schema, check that it is consistent with itself, and check
-- that every existing tuple is valid under it.  On success, the 'rsDefMap'
-- for the new schema.
checkNewSchema :: [RelationTuple] -> SchemaText -> Either UpdateSchemaError (Map Text RelPerm)
checkNewSchema tuples (SchemaText schemaText) =
  case parseSchema schemaText of
    (Left e) -> Left (SchemaParseError e)
    (Right newSchema) ->
      case validateSchema newSchema of
        errs@(_ : _) -> Left (SchemaInconsistent errs)
        [] ->
          let newDefMap = mkDefMap (definitions newSchema)
          in case partition isValid (map (validate newDefMap) tuples) of
               (_, [])       -> Right newDefMap
               (_, rejected) -> Left (SchemaDoesNotAllowRelations [ e | NotValid e <- rejected ])

data UpdateSchemaResult
  = SchemaUpdated SchemaId
  | SchemaUpdateFailed UpdateSchemaError
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy UpdateSchemaResult where version = 1 ; kind = base

-- | Make a schema that 'checkNewSchema' accepted the current one, with the
-- 'rsDefMap' it returned, recording it in the history and the log.  Returns
-- its 'SchemaId'.  Shared by every event that changes the schema.
applySchema :: UTCTime -> SchemaText -> Map Text RelPerm -> Text -> RebacState -> (SchemaId, RebacState)
applySchema now newSchemaText newDefMap comment rs =
  ( rsSchemaId rs
  , rs { rsSchema        = newSchemaText
       , rsSchemaHistory = Map.insert (rsSchemaId rs) newSchemaText (rsSchemaHistory rs)
       , rsDefMap        = newDefMap
       , rsLog           = rle : (rsLog rs)
       , rsSchemaId      = succ (rsSchemaId rs)
       }
  )
  where
    rle = SchemaUpdate { rleSchemaUpdateTimestamp = now
                       , rleSchemaId              = rsSchemaId rs
                       , rleComment               = comment
                       , rleSchemaFromTxId        = rsTxId rs
                       }

updateSchema :: UTCTime -> SchemaText -> Text -> Update RebacState UpdateSchemaResult
updateSchema now newSchemaText comment =
  do rs <- get
     case checkNewSchema (toTuples (rsTuples rs)) newSchemaText of
       (Left err) -> pure (SchemaUpdateFailed err)
       (Right newDefMap) ->
         do let (sid, rs') = applySchema now newSchemaText newDefMap comment rs
            put rs'
            pure (SchemaUpdated sid)

checkSchema :: SchemaText -> Query RebacState (Maybe UpdateSchemaError)
checkSchema newSchemaText =
  do rs <- ask
     pure $ either Just (const Nothing) (checkNewSchema (toTuples (rsTuples rs)) newSchemaText)


getDefMap :: Query RebacState (Map Text RelPerm)
getDefMap = rsDefMap <$> ask

data AddRelationTupleError
  = RelationTupleNotValid ValidationError
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy AddRelationTupleError

ppAddRelationTupleError :: AddRelationTupleError -> Doc
ppAddRelationTupleError (RelationTupleNotValid ve) =
  ppValidationError ve

-- | add a 'RelationTuple' to the database
--
-- If the 'RelationTuple' already exists in the 'rsTuples' the 'rsTuple' cache will not be modified, but the
-- 'RelationTuple' will still be added to the 'rsLog'
--
-- FIXME: add a precondition check. If the precondition fails, no modifications are performed.
addRelationTuple :: RelationTuple -- ^ 'RelationTuple' to add
                 -> UTCTime       -- ^ approximate wall time this relation tuple was added
                 -> Text          -- ^ a human readable comment explaining how/why this relation got added
                 -> Update RebacState (Either AddRelationTupleError RelationLogEntry)
addRelationTuple rt now comment =
  do rs <- get
     case validate (rsDefMap rs) rt of
       Valid ->
         do let rle = (RelationLogEntry { rleTimestamp     = now
                                         , rleRelationTuple = rt
                                         , rleAction        = RLEAdd
                                         , rleComment       = comment
                                         , rleTxId          = rsTxId rs
                                         })
            put $ rs { rsTuples = if rt `elemTuple` (rsTuples rs) then (rsTuples rs) else insertTuple rt (rsTuples rs)  -- don't insert into rsTuples cache if it already exists
                     , rsLog = rle : (rsLog rs)
                     , rsTxId = succRelationIxId (rsTxId rs)
                     }
            pure (Right rle)
       (NotValid ve) ->
         pure (Left (RelationTupleNotValid ve))



addRelationTuples :: [RelationTuple] -- ^ 'RelationTuple's to add
                  -> UTCTime         -- ^ approximate wall time this relation tuple was added
                  -> Text            -- ^ a human readable comment explaining how/why this relation got added
                  -> Update RebacState [RelationLogEntry]
addRelationTuples [] now comment = pure []
addRelationTuples rts now comment =
  do rs <- get
     let rles = [ RelationLogEntry { rleTimestamp     = now
                                   , rleRelationTuple = rt
                                   , rleAction        = RLEAdd
                                   , rleComment       = comment
                                   , rleTxId          = rsTxId rs
                                   } | rt <- rts ]
     put $ rs { rsTuples = foldl' (\idx rt -> if rt `elemTuple` idx then idx else insertTuple rt idx) (rsTuples rs) rts
              , rsLog = rles ++ (rsLog rs)
              , rsTxId = succRelationIxId (rsTxId rs)
              }
     pure rles

-- | remove a 'RelationTuple' from the database
--
-- This only removes tuples that are an exact match.
--
-- If the 'RelationTuple' does not exist in the 'rsTuples' the 'rsTuple' cache will not be modified and no error will be returned.
-- But the 'RelationTuple' removal will still be added to the 'rsLog'.
--
removeRelationTuple :: RelationTuple -- ^ 'RelationTuple' to remove
                    -> UTCTime       -- ^ approximate wall time this relation tuple was removed
                    -> Text          -- ^ a human readable comment explaining how/why this relation got added
                    -> Update RebacState RelationLogEntry
removeRelationTuple rt now comment =
  do rs <- get
     let rle = (RelationLogEntry { rleTimestamp     = now
                                 , rleRelationTuple = rt
                                 , rleAction        = RLERemove
                                 , rleComment       = comment
                                 , rleTxId          = rsTxId rs
                                 })
     put $ rs { rsTuples = deleteTuple rt (rsTuples rs)
              , rsLog    = rle : (rsLog rs)
              , rsTxId   = succRelationIxId (rsTxId rs)
              }
     pure rle

removeRelationTuplesByTag :: Tag      -- ^ remove *any* tuple with this tag
                          -> UTCTime  -- ^ approximate wall time this relation tuple was removed
                          -> Text     -- ^ a human readable comment explaining how/why these relations got removed
                          -> Update RebacState [RelationLogEntry]
removeRelationTuplesByTag tag now comment =
  do rs <- get
     let remove = filter (hasTag tag) (toTuples (rsTuples rs))
     let rles = [ RelationLogEntry { rleTimestamp     = now
                                   , rleRelationTuple = rt
                                   , rleAction        = RLERemove
                                   , rleComment       = comment
                                   , rleTxId          = rsTxId rs
                                   } | rt <- remove ]
     put $ rs { rsTuples = foldl' (flip deleteTuple) (rsTuples rs) remove
              , rsLog    = rles ++ (rsLog rs)
              , rsTxId   = succRelationIxId (rsTxId rs)
              }
     pure rles

-- | this provides an atomic way to remove all the tuples associated with a 'tag' and insert new tuples for that 'tag'.
--
-- NOTE: it is up to the caller to ensure that the new tuples have the same tag
replaceRelationTuplesByTag :: Tag -- ^ remove *any* tuples with this tag
                           -> [RelationTuple] -- ^ add these new tuples. They *should* have the same tag, but that is not enforced
                           -> UTCTime -- ^ approximate wall time this operation was performed
                           -> Text -- ^ humman readable comment on why this happened
                           -> Update RebacState [RelationLogEntry]
replaceRelationTuplesByTag tag rts now comment =
  do removedRLEs <- removeRelationTuplesByTag tag now comment
     addedRLEs   <- addRelationTuples rts now comment
     pure (removedRLEs ++  addedRLEs)

-- | Can the subject do this to the resource?  Evaluated against the stored
-- tuples and schema as one consistent snapshot, using the stored index,
-- without copying the tuples out.
checkPermission :: Object 'NoWildcard -> Permission -> Object 'NoWildcard -> Maybe POSIXTime -> Query RebacState Access
checkPermission res perm subj now =
  do rs <- ask
     pure $ checkIndexed (rsDefMap rs) (rsTuples rs) res perm subj now

-- | The resources of a type on which the subject has a permission, such as
-- every report a user can view, evaluated like 'checkPermission' against the
-- stored tuples and schema.  See 'Check.lookupResources' for its cost.
lookupResources :: ObjectType -> Permission -> Object 'NoWildcard -> Maybe POSIXTime -> Query RebacState [Object 'NoWildcard]
lookupResources ty perm subj now =
  do rs <- ask
     pure $ Check.lookupResources (rsDefMap rs) (rsTuples rs) ty perm subj now

-- | get all the 'RelationTuple' from the database, grouped by resource
getRelationTuples :: Query RebacState [ RelationTuple ]
getRelationTuples =
  do rs <- ask
     pure $ toTuples (rsTuples rs)

-- | get all the 'RelationTupleEntry' from the database
getRelationLog :: Query RebacState [ RelationLogEntry ]
getRelationLog =
  do rs <- ask
     pure $ rsLog rs

-- | get the 'SchemaId' that was active when relation transaction 'txId' was performed
getSchemaIdForTxId :: RelationTxId -> Query RebacState (Maybe SchemaId)
getSchemaIdForTxId txId =
  do rs <- ask
     pure $ schemaIdForTxId txId (rsLog rs)

-- * Reconciling with the plugins' schema fragments

-- | Which seed tuples still need adding.  A seed tuple is added only if it
-- has never been added or removed before, according to 'rsLog', so it is
-- added once, and if it is removed later, for instance to make page 1
-- private, it stays removed.  Each group keeps its label, and a tuple in
-- more than one group is only added with the first.
newSeedTuples :: RebacState -> [(Text, [RelationTuple])] -> [(Text, [RelationTuple])]
newSeedTuples rs groups = snd (mapAccumL pick [] groups)
  where
    logged = [ rt | RelationLogEntry { rleRelationTuple = rt } <- rsLog rs ]
    pick taken (label, seeds) =
      let new = [ rt | rt <- nub seeds, rt `notElem` logged, not (rt `elemTuple` rsTuples rs), rt `notElem` taken ]
      in (taken ++ new, (label, new))

-- | Add groups of tuples as one transaction, each tuple logged with its
-- group's label as the comment.
applySeedTuples :: UTCTime -> [(Text, [RelationTuple])] -> RebacState -> RebacState
applySeedTuples now groups rs
  | null (concatMap snd groups) = rs
  | otherwise =
      rs { rsTuples = foldl' (flip insertTuple) (rsTuples rs) (concatMap snd groups)
         , rsLog    = [ RelationLogEntry now rt RLEAdd label (rsTxId rs) | (label, rts) <- reverse groups, rt <- rts ] ++ rsLog rs
         , rsTxId   = succRelationIxId (rsTxId rs)
         }

data ReconcileError
  = ReconcileSchemaRejected UpdateSchemaError         -- ^ the merged schema was rejected, as 'UpdateSchema' would
  | ReconcileSeedsInvalid Text [ValidationError]      -- ^ the seed tuples with this label are not valid under the merged schema
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy ReconcileError where version = 1 ; kind = base

ppReconcileError :: ReconcileError -> Doc
ppReconcileError (ReconcileSchemaRejected err) = ppUpdateSchemaError err
ppReconcileError (ReconcileSeedsInvalid label errs) = PP.text ("Not valid: " ++ Text.unpack label) $+$ PP.nest 2 (PP.vcat (map ppValidationError errs))

data ReconcileResult = ReconcileResult
  { reconciledSchemaId :: Maybe SchemaId    -- ^ the new schema's id, if it changed
  , reconciledSeeds    :: [RelationTuple]   -- ^ the seed tuples that were added
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance SafeCopy ReconcileResult where version = 1 ; kind = base

-- | Make @newSchemaText@ the current schema, if it is not already, and add
-- the seed tuples that have not been seen before (see 'newSeedTuples'), all
-- in one transaction.  This is what 'Clckwrks.Rebac.Reconcile' does at
-- startup with the plugins' merged fragments and their seed tuples.
--
-- Everything is checked before anything is changed: the schema as
-- 'UpdateSchema' would check it, and the seed tuples against the new
-- schema.  If anything is rejected nothing changes.
reconcileSchema :: UTCTime
                -> SchemaText                 -- ^ the merged schema
                -> Text                       -- ^ comment for the schema update
                -> [(Text, [RelationTuple])]  -- ^ seed tuples, in groups, each with a label used as their comment
                -> Update RebacState (Either ReconcileError ReconcileResult)
reconcileSchema now newSchemaText comment seedGroups =
  do rs <- get
     let schemaStep
           | rsSchema rs == newSchemaText = Right (Nothing, rs)
           | otherwise =
               case checkNewSchema (toTuples (rsTuples rs)) newSchemaText of
                 (Left err)        -> Left (ReconcileSchemaRejected err)
                 (Right newDefMap) -> let (sid, rs') = applySchema now newSchemaText newDefMap comment rs in Right (Just sid, rs')
     case schemaStep of
       (Left err) -> pure (Left err)
       (Right (mSid, rs')) ->
         let groups = newSeedTuples rs' seedGroups
             invalid = [ (label, errs) | (label, rts) <- groups
                                       , let errs = [ e | NotValid e <- map (validate (rsDefMap rs')) rts ]
                                       , not (null errs) ]
         in case invalid of
              ((label, errs) : _) -> pure (Left (ReconcileSeedsInvalid label errs))
              [] ->
                do put (applySeedTuples now groups rs')
                   pure (Right (ReconcileResult mSid (concatMap snd groups)))

makeAcidic ''RebacState
 [ 'addRelationTuple
 , 'addRelationTuples
 , 'reconcileSchema
 , 'removeRelationTuple
 , 'removeRelationTuplesByTag
 , 'replaceRelationTuplesByTag
 , 'getRelationTuples
 , 'getRelationLog
 , 'checkPermission
 , 'lookupResources
 , 'getSchemaIdForTxId
 , 'getDefMap
 , 'getSchema
 , 'getSchemaById
 , 'checkSchema
 , 'updateSchema
 ]



-- * Initial REBAC schema and relations for clckwrks

-- | clckwrks' own part of the rebac schema, checked at compile time.  The
-- clck plugin registers it with 'Clckwrks.Monad.addRebacFragment', and at
-- startup it is merged with the other plugins' fragments to make the
-- stored schema, see 'Clckwrks.Rebac.Reconcile'.
clckwrksFragment :: SchemaFragment
clckwrksFragment =
  [schemaFragment|
         definition user {}

         definition platform {
           relation administrator: user

           permission super_admin = administrator
         }

         /* the core clckwrks plugins */
         definition clck {
           relation controller: platform
           permission admin = controller->super_admin
         }

         /* controls access to the REBAC API

            For example, can a user query the underlying schema and relations database.
         */
         definition rebac_api {
           relation controller: platform

           permission get    = controller->super_admin
           permission modify = controller->super_admin
         }

         /*
         controls access to various pages in the REBAC admin panel.

         While the rebac_api might prevent the user from seeing any
         data in the database should they visit the schema or relations page,
         we can create a better user experience if we don't even let them see
         the page.
         */
         definition rebac_url {
           relation controller: platform

           permission rebac_view = controller->super_admin
         }

         definition usergroup {
           relation direct_member: user | usergroup#member

           permission member = direct_member
         }

         /* schema for clckwrks-plugin-page */
         definition page {
           relation admin: user
           relation viewer: user | usergroup#member | user:*

           permission view = viewer + admin
           permission edit = admin
         }

         /* schema for clckwrk-plugin-stripe */
         definition stripe {
         }

 |]

-- | the text of 'clckwrksFragment', the schema a new database starts with
clckwrksSchema :: SchemaText
clckwrksSchema = SchemaText (fragmentText clckwrksFragment)

    {-
         page:1#viewer@usergroup:subscribers#member
         page:1#admin@user:1
         page:1#viewer@user:2
         usergroup:subscribers#direct_member@user:1
      -}

clckwrksRels :: [ RelationTuple ]
clckwrksRels =
--          page:admin#controller@platform:clckwrks                     # platform admins are also page admins
  [rels|
         platform:clckwrks#administrator@user:1                      # security warning - the first account created automatically gets admin access
         clck:admin#controller@platform:clckwrks                     # platform admins are also clck admins
         page:1#viewer@user:*
         rebac_api:schema#controller@platform:clckwrks
         rebac_api:relations#controller@platform:clckwrks
         rebac_api:relation_log#controller@platform:clckwrks
         rebac_url:schema_panel#controller@platform:clckwrks
         rebac_url:relations_panel#controller@platform:clckwrks
         rebac_url:relation_log_panel#controller@platform:clckwrks
  |]


-- | initialRebacState
--
-- we want to populate the relation log to include the entries that
-- were originally added by `initialRebacState`. Those entries will
-- have a timestamp of epoch time.
initialRebacState :: IO RebacState
initialRebacState =
  do now <- getCurrentTime
     pure $ RebacState
       { rsSchema    = clckwrksSchema
       , rsSchemaHistory = Map.singleton (SchemaId 1) clckwrksSchema
       , rsDefMap    = case parseSchema (unSchemaText clckwrksSchema) of
                         (Right s) -> mkDefMap (definitions s)
       , rsTuples    = fromTuples clckwrksRels
       , rsLog       = (map (fakeLog now) clckwrksRels) ++ [SchemaUpdate now (SchemaId 1) "initial schema" (RelationTxId 0)]
       , rsTxId      = RelationTxId 1
       , rsSchemaId  = SchemaId 2
       }
  where
    fakeLog :: UTCTime -> RelationTuple -> RelationLogEntry
    fakeLog now rt = RelationLogEntry now rt RLEAdd "initial rebac state" (RelationTxId 0)

    {-
initialRebacState :: SchemaText -> RebacState
initialRebacState schemaText = RebacState
  { rsSchema = schemaText
  , rsDefMap = mkDefMap (definitions schema)
  , rsTuples = clckwrksRels
  , rsLog    = map fakeLog clckwrksRels
  , rsTxId   = RelationTxId 1
  }
  where
    fakeLog :: RelationTuple -> RelationLogEntry
    fakeLog rt = RelationLogEntry (posixSecondsToUTCTime 0) rt RLEAdd "initial rebac state" (RelationTxId 0)
-}

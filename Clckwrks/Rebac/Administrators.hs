{-# LANGUAGE DataKinds, OverloadedStrings #-}
-- | rebac's platform administrators, the subjects of
-- @platform:clckwrks#administrator@ tuples, kept the same as the users
-- with the 'Administrator' role, until that role is replaced by rebac.
--
-- clckwrks makes nobody an administrator by itself: the role is given
-- with clckwrks-cli, which calls 'syncAdministrators' after changing a
-- role, so the tuples follow; so does the Edit Profile Data page, and
-- the server at startup ('Clckwrks.Plugin.clckInit'), which repairs a
-- database the CLI has not.  The tuples it writes are tagged
-- 'administratorsTag', and any other administrator tuple, such as the
-- one clckwrks used to seed for user 1, is removed.
--
-- Takes the databases rather than a server's state, so that it works
-- on the local states of a running server and on the remote states
-- clckwrks-cli opens through the server's sockets.  It uses only
-- queries and updates older servers also have.
module Clckwrks.Rebac.Administrators
    ( administratorsTag
    , administratorTuple
    , isAdministratorTuple
    , AdministratorsChange(..)
    , syncAdministrators
    ) where

import AccessControl.Relation      (Object(..), ObjectId(..), ObjectType(..), ObjectWildcard(NoWildcard), Relation(..), RelationTuple(..), Tag(..), ToObject(toObject), toWildcard)
import Clckwrks.ProfileData.Acid   (GetRoles(..), GetUserIdDisplayNames(..), ProfileDataState)
import Clckwrks.ProfileData.Types  (Role(Administrator))
import Clckwrks.Rebac.Acid         (GetRelationTuples(..), RebacState, RemoveRelationTuple(..), SetTaggedTuples(..), SetTaggedTuplesError, TaggedGroupsOutOfStep(..))
import Control.Monad               (filterM, forM_)
import Data.Acid                   (AcidState, query, update)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import Data.Text                   (Text)
import Data.Time.Clock             (getCurrentTime)
import Data.UserId                 (UserId)

-- | The tag on the administrator tuples 'syncAdministrators' writes.
administratorsTag :: Tag
administratorsTag = Tag "role:administrator"

platform :: Object 'NoWildcard
platform = Object (ObjectType "platform") (ObjectId "clckwrks")

-- | @platform:clckwrks#administrator@user:<uid>@, tagged 'administratorsTag'.
administratorTuple :: UserId -> RelationTuple
administratorTuple uid =
    RelationTuple { resource        = platform
                  , relation        = Relation "administrator"
                  , subject         = toWildcard (toObject uid)
                  , subjectRelation = Nothing
                  , tag             = Just administratorsTag
                  , expiration      = Nothing
                  }

-- | Whether a tuple makes its subject a platform administrator, whatever
-- its tag.
isAdministratorTuple :: RelationTuple -> Bool
isAdministratorTuple rt = resource rt == platform && relation rt == Relation "administrator"

-- | The administrator tuples a sync added and removed.  A user whose
-- untagged tuple was replaced by a tagged one is in neither.
data AdministratorsChange = AdministratorsChange
    { administratorsAdded   :: [RelationTuple]
    , administratorsRemoved :: [RelationTuple]
    }
    deriving (Eq, Show)

-- | Make the platform administrators in rebac exactly the users with the
-- 'Administrator' role.  Writes nothing if they already are.  Fails,
-- changing nothing, if the stored schema does not allow the tuples.
syncAdministrators :: AcidState ProfileDataState
                   -> AcidState RebacState
                   -> Text -- ^ why, for the relation log
                   -> IO (Either [SetTaggedTuplesError] AdministratorsChange)
syncAdministrators profileData rebac why =
    do users  <- map fst <$> query profileData GetUserIdDisplayNames
       admins <- filterM (\uid -> Set.member Administrator <$> query profileData (GetRoles uid)) users
       stored <- filter isAdministratorTuple <$> query rebac GetRelationTuples
       let wanted = map administratorTuple admins
           -- administrator tuples outside the group: the old seed, or
           -- one added by hand
           strays = [ rt | rt <- stored, tag rt /= Just administratorsTag ]
       out <- query rebac (TaggedGroupsOutOfStep [(administratorsTag, wanted)])
       now <- getCurrentTime
       r <- if null out
              then pure (Right ())
              else fmap (const ()) <$> update rebac (SetTaggedTuples administratorsTag wanted now why)
       case r of
         Left errs -> pure (Left errs)
         Right () ->
             do forM_ strays $ \rt -> update rebac (RemoveRelationTuple rt now why)
                let before = Set.fromList (map subject stored)
                    after  = Set.fromList (map subject wanted)
                pure $ Right $ AdministratorsChange
                         { administratorsAdded   = [ rt | rt <- wanted, not (Set.member (subject rt) before) ]
                         , administratorsRemoved = Map.elems (Map.fromList [ (subject rt, rt) | rt <- stored, not (Set.member (subject rt) after) ])
                         }

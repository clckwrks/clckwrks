{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RankNTypes, RecordWildCards, ScopedTypeVariables, UndecidableInstances, OverloadedStrings, TemplateHaskell #-}
module Clckwrks.AccessControl where

import AccessControl.Check           (Access(..), check)
import AccessControl.Schema          (KnownPermission, Permission(..), ToPermission(..), ppPermission)
import AccessControl.Relation        ( Relation(..), ToRelation(..), ToObject(..), Object(..), ObjectId(..), ObjectType(..), ppObject)
import Clckwrks.Authenticate.Plugin  (getUserId)
import Clckwrks.Monad
import Clckwrks.Rebac.Acid           (RebacState, GetDefMap(..), GetRelationTuples(..), GetSchema(..))
import Control.Monad.State           (get)
import Control.Monad.Trans           (MonadIO(..))
import Clckwrks.Types
import Clckwrks.Unauthorized         (unauthorizedPage)
import Data.Data                     (Data)
import Data.SafeCopy                 (SafeCopy)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy      as TL
import Data.Time.Clock.POSIX         (POSIXTime)
import Data.Typeable                 (Typeable)
import Data.UserId                   (UserId(..))
import Happstack.Server              (Happstack, askRq, escape, rqUri, rqQuery)
import GHC.Generics                  (Generic)

data AccessList = AccessList
  { allowAny         :: Bool
  , allowUserIds     :: [ UserId ]
  , allowUsergroups  :: [ Text ]
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance SafeCopy AccessList

emptyAccessList :: AccessList
emptyAccessList = AccessList False [] []

-- | find out if the current user has permession to access a resource
checkAccess :: (KnownPermission resource permission (Maybe UserId), Happstack m, MonadIO m) => resource -> permission -> Maybe POSIXTime -> ClckT url m Access
checkAccess res perm now =
  do mu <- getUserId
     rts <- query GetRelationTuples
     scm <- query GetDefMap
     pure $ check scm rts (toObject res) (toPermission perm) (toObject mu) now
--     query (Check (toObject res) (toPermission perm) (toObject mu))
{-
     case mu of
       Nothing ->
             query (Check (toObject res) (toPermission perm) (Object (ObjectType "anonymous") (ObjectId "anonymous")))
       (Just uid) ->
-}

-- | assert that a user has permission to access a resource. If this assertion is wrong, show an 'unauthorized access' page
assertAccess ::(KnownPermission resource permission (Maybe UserId), Happstack m, MonadIO m) => resource -> permission -> Maybe POSIXTime -> ClckT url m ()
assertAccess res perm now =
  do a <- checkAccess res perm now
     case a of
       Allowed -> pure ()
       NotAllowed reasons ->
         do rq <- askRq
            mu <- getUserId
            escape $ do setRedirectCookie (rqUri rq ++ rqQuery rq)
                        unauthorizedPage  ("You do not have permission to access this resource. ") {- <>
                                           "resource = " <> (TL.pack (show $ ppObject (toObject res))) <>
                                           ", permission = " <> (TL.pack (show $ ppPermission (toPermission perm))) <>
                                           ", subject = " <> (TL.pack (show $ ppObject (toObject mu))) <>
                                           ", reasons = " <> (TL.pack $ show reasons) :: TL.Text) -}

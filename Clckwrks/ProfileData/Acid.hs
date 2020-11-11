{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module Clckwrks.ProfileData.Acid
    ( ProfileDataState(..)
    , initialProfileDataState
    , SetProfileData(..)
    , GetProfileData(..)
    , NewProfileData(..)
    , UpdateProfileData(..)
    , HasRole(..)
    , GetRoles(..)
    , AddRole(..)
    , RemoveRole(..)
    ) where

import Clckwrks.ProfileData.Types  (ProfileData(..), Role(..), defaultProfileDataFor)
import Control.Applicative         ((<$>))
import Control.Monad.Reader        (ask)
import Control.Monad.State         (get, put)
import Data.Acid                   (Update, Query, makeAcidic)
import Data.Data                   (Data, Typeable)
import Data.IxSet                  (IxSet, (@=), empty, getOne, insert, updateIx, toList)
import Data.SafeCopy               (base, deriveSafeCopy)
import qualified Data.Set          as Set
import           Data.Set          (Set)
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Data.UserId                 (UserId(..))

data ProfileDataState = ProfileDataState
    { profileData :: IxSet ProfileData
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''ProfileDataState)

initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profileData = empty }

setProfileData :: ProfileData
               -> Update ProfileDataState ()
setProfileData pd =
    do pds@(ProfileDataState{..}) <- get
       put $ pds { profileData = updateIx (dataFor pd) pd profileData }


getProfileData :: UserId
               -> Query ProfileDataState ProfileData
getProfileData uid =
    do ProfileDataState{..} <- ask
       case getOne $ profileData @= uid of
         (Just pd) -> return pd
         Nothing -> return (defaultProfileDataFor uid)

updateProfileData :: ProfileData
                  -> Update ProfileDataState ()
updateProfileData pd =
    do ps <- get
       put $ ps { profileData = updateIx (dataFor pd) pd (profileData ps) }

modifyProfileData :: (ProfileData -> ProfileData)
                  -> UserId
                  -> Update ProfileDataState ()
modifyProfileData fn uid =
    do ps@(ProfileDataState {..}) <- get
       case getOne $ profileData @= uid of
         Nothing   ->
           do let pd' = fn (defaultProfileDataFor uid)
              put ps { profileData = insert pd' profileData }
         (Just pd) ->
           do let pd' = fn pd
              put ps { profileData = updateIx (dataFor pd') pd' profileData }



-- | create the profile data, but only if it is missing
newProfileData :: ProfileData
               -> Update ProfileDataState (ProfileData, Bool)
newProfileData pd =
    do pds@(ProfileDataState {..}) <- get
       case getOne (profileData @= (dataFor pd)) of
         Nothing -> do put $ pds { profileData = updateIx (dataFor pd) pd profileData }
                       return (pd, True)
         (Just pd') -> return (pd', False)

getRoles :: UserId
         -> Query ProfileDataState (Set Role)
getRoles uid =
    do profile <- getProfileData uid
       return (roles profile)

hasRole :: UserId
        -> Set Role
        -> Query ProfileDataState Bool
hasRole uid role =
    do profile <- getProfileData uid
       return (not $ Set.null $ role `Set.intersection` roles profile)

addRole :: UserId
        -> Role
        -> Update ProfileDataState ()
addRole uid role =
    modifyProfileData fn uid
    where
      fn profileData = profileData { roles = Set.insert role (roles profileData) }

removeRole :: UserId
           -> Role
           -> Update ProfileDataState ()
removeRole uid role =
    modifyProfileData fn uid
    where
      fn profileData = profileData { roles = Set.delete role (roles profileData) }

$(makeAcidic ''ProfileDataState
  [ 'setProfileData
  , 'getProfileData
  , 'newProfileData
  , 'updateProfileData
  , 'getRoles
  , 'hasRole
  , 'addRole
  , 'removeRole
  ])

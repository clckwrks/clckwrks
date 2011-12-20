{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module Clckwrks.ProfileData.Acid
    ( ProfileDataState(..)
    , initialProfileDataState
    , SetProfileData(..)
    , GetProfileData(..)
    , NewProfileData(..)
    ) where

import Clckwrks.ProfileData.Types (ProfileData(..))
import Control.Monad.Reader       (ask)
import Control.Monad.State        (get, put)
import Data.Acid                  (Update, Query, makeAcidic)
import Data.Data                  (Data, Typeable)
import Data.IxSet                 (IxSet, (@=), empty, getOne, updateIx)
import Data.SafeCopy              (base, deriveSafeCopy)
import Happstack.Auth             (UserId(..))

data ProfileDataState = ProfileDataState
    { profileData :: IxSet ProfileData
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'base ''ProfileDataState)

initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profileData = empty }

setProfileData :: ProfileData -> Update ProfileDataState ()
setProfileData pd =
    do pds@(ProfileDataState{..}) <- get
       put $ pds { profileData = updateIx (dataFor pd) pd profileData }

getProfileData :: UserId -> Query ProfileDataState (Maybe ProfileData)
getProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profileData @= uid

-- | create the profile data, but only if it is missing
newProfileData :: ProfileData -> Update ProfileDataState ProfileData
newProfileData pd = 
    do pds@(ProfileDataState {..}) <- get       
       case getOne (profileData @= (dataFor pd)) of
         Nothing -> do put $ pds { profileData = updateIx (dataFor pd) pd profileData }
                       return pd
         (Just pd') -> return pd'


$(makeAcidic ''ProfileDataState 
  [ 'setProfileData
  , 'getProfileData
  , 'newProfileData
  ])

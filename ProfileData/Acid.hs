{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, RecordWildCards, TypeFamilies #-}
module ProfileData.Acid
    ( ProfileDataState(..)
    , initialProfileDataState
    , SetProfileData(..)
    , GetProfileData(..)
    , NewProfileData(..)
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Data
import Data.IxSet
import Data.SafeCopy
import Happstack.Auth (UserId(..))
import ProfileData.Types

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

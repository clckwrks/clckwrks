{-# LANGUAGE RecordWildCards #-}
module Clckwrks.ProfileData.API
    ( getProfileData
    , getUsername
    , whoami
    )  where

import Clckwrks.Acid  (Acid(..))
import Clckwrks.Monad
import Clckwrks.ProfileData.Acid
import Clckwrks.ProfileData.Types
import Control.Applicative         ((<$>))
import Control.Monad.State         (get)
import Data.Text                   (Text)
import Happstack.Auth              (UserId(..))

getProfileData :: UserId -> Clck url (Maybe ProfileData)
getProfileData uid = query (GetProfileData uid)

getUsername :: UserId -> Clck url (Maybe Text)
getUsername uid =
    query (GetUsername uid)

whoami :: Clck url (Maybe UserId)
whoami =
    do -- Acid{..} <- acidState <$> get
       getUserId


{-# LANGUAGE RecordWildCards #-}
module Clckwrks.ProfileData.API
    ( getProfileData
    , whoami
    )  where

import Clckwrks.Acid  (Acid(..))
import Clckwrks.Monad
import Clckwrks.ProfileData.Acid
import Clckwrks.ProfileData.Types
import Control.Applicative        ((<$>))
import Control.Monad.State        (get)
import Happstack.Auth

getProfileData :: UserId -> Clck url (Maybe ProfileData)
getProfileData uid = query (GetProfileData uid)

whoami :: Clck url (Maybe UserId)
whoami =
    do Acid{..} <- acidState <$> get
       getUserId acidAuth acidProfile


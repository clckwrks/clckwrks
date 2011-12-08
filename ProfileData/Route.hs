{-# LANGUAGE RecordWildCards #-}
module ProfileData.Route where

import Control.Monad.State (get)
import Clckwrks
import Data.Set (singleton)
import Happstack.Auth 
import ProfileData.Acid
import ProfileData.URL (ProfileDataURL(..))
import ProfileData.Types

routeProfileData :: ProfileDataURL -> Clck ProfileDataURL Response
routeProfileData url =
    case url of
      CreateNewProfileData ->
          do Acid{..} <- acidState <$> get
             mUserId <- getUserId acidAuth acidProfile
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do let profileData = emptyProfileData { dataFor = userId 
                                                         , roles   = singleton Visitor
                                                         }
                      update (NewProfileData profileData)
                      seeOther "/" (toResponse "/")

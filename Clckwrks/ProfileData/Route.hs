{-# LANGUAGE RecordWildCards #-}
module Clckwrks.ProfileData.Route where

import Clckwrks
import Clckwrks.ProfileData.Acid
import Clckwrks.ProfileData.EditProfileData (editProfileDataPage)
import Clckwrks.ProfileData.EditNewProfileData (editNewProfileDataPage)
import Clckwrks.ProfileData.EditProfileDataFor (editProfileDataForPage)
import Clckwrks.ProfileData.URL   (ProfileDataURL(..))
import Clckwrks.ProfileData.Types
import Control.Monad.State (get)
import Data.Set (singleton)
import Data.Text (Text)

routeProfileData :: ProfileDataURL -> Clck ProfileDataURL Response
routeProfileData url =
    case url of
      CreateNewProfileData ->
          do mUserId <- getUserId
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do (_, new) <- update (NewProfileData (defaultProfileDataFor userId))
                      if new
                        then seeOtherURL EditNewProfileData
                        else do mRedirectCookie <- getRedirectCookie -- first priority for the redirect is if the user was trying to get somewhere already
                                case mRedirectCookie of
                                  (Just u) -> seeOther u (toResponse ())
                                  Nothing  -> do
                                    mRedirect <- query GetLoginRedirect
                                    case mRedirect of
                                      (Just url) -> seeOther url (toResponse ()) -- second priority is if the redirect after login is set
                                      Nothing    -> do
                                        seeOtherURL EditProfileData -- third priority is the edit profile data page

      EditProfileData ->
             do editProfileDataPage url
      EditNewProfileData ->
             do editNewProfileDataPage url
      EditProfileDataFor u ->
             do editProfileDataForPage url u


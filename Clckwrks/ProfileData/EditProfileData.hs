{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.ProfileData.EditProfileData where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.ProfileData.Acid (GetProfileData(..), SetProfileData(..), profileDataErrorStr)
import Data.Text                (Text, pack)
import Data.Maybe               (fromMaybe)
import qualified Data.Text      as Text
import Happstack.Auth           (UserId)
import Text.Reform              ((++>), transformEitherM)
import Text.Reform.HSP.Text     (form, inputText, inputSubmit, label, fieldset, ol, li)
import Text.Reform.Happstack    (reform)

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

editProfileDataPage :: ProfileDataURL -> Clck ProfileDataURL Response
editProfileDataPage here =
    do mUid <- getUserId
       case mUid of
         Nothing -> internalServerError $ toResponse $ "Unable to retrieve your userid"
         (Just uid) ->
             do mpd <- query (GetProfileData uid)
                case mpd of
                  Nothing ->
                      internalServerError $ toResponse $ "Missing profile data for " ++ show uid
                  (Just pd) ->
                      do action <- showURL here
                         template "Edit Profile Data" () $
                               <%>
                                <% reform (form action) "epd" updated Nothing (profileDataFormlet pd) %>
                               </%>
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

profileDataFormlet :: ProfileData -> ClckForm ProfileDataURL ()
profileDataFormlet pd@ProfileData{..} =
    ((,) <$> (li $ label "username:" )        ++> (li $ inputText username)
         <*> (li $ label" email (optional):") ++> (li $ inputText (fromMaybe Text.empty email))
         <* inputSubmit (pack "update"))
    `transformEitherM` updateProfileData
    where
      updateProfileData :: (Text, Text) -> Clck ProfileDataURL (Either ClckFormError ())
      updateProfileData (usrnm, eml) =
              if Text.null usrnm
                 then do return (Left EmptyUsername)
                 else do let newPd = pd { username = usrnm
                                        , email    = if Text.null eml then Nothing else (Just eml)
                                        }
                         merr <- update (SetProfileData newPd)
                         case merr of
                           Nothing    -> return $ Right ()
                           (Just err) -> return $ Left (PDE err)

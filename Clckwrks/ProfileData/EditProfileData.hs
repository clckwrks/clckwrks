{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.ProfileData.EditProfileData where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.FormPart        (FormDF, fieldset, ol, li, multiFormPart)
import Clckwrks.ProfileData.Acid (GetProfileData(..), SetProfileData(..), profileDataErrorStr)
import Data.Text                (Text)
import qualified Data.Text      as Text
import Happstack.Auth           (UserId)
import Text.Digestive           (Transformer, (++>), transform, transformEitherM)
import Text.Digestive.HSP.Html4 (inputText, label, submit)

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
                                <% multiFormPart "epd" action updated Nothing (profileDataFormlet pd) %>
                               </%>
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

profileDataFormlet :: ProfileData -> FormDF (Clck ProfileDataURL) ()
profileDataFormlet pd@ProfileData{..} =
    (fieldset $
       ol $ (,) <$> ((li $ label "username:")                ++>
                        (li $ inputText (Just username)))
                <*> ((li $ label "email (optional):")  ++>
                        (li $ inputText email))
                <*  submit "update")
    `transform` updateProfileData
    where
      updateProfileData :: (Functor m, MonadIO m) => Transformer (ClckT url m) String (Text, Text) ()
      updateProfileData =
          transformEitherM $ \(usrnm, eml) ->
              if Text.null usrnm
                 then do return (Left $ "Username can not be empty.")
                 else do let newPd = pd { username = usrnm
                                        , email    = if Text.null eml then Nothing else (Just eml)
                                        }
                         merr <- update (SetProfileData newPd)
                         case merr of
                           Nothing    -> return $ Right ()
                           (Just err) -> return $ Left (profileDataErrorStr err)

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.ProfileData.EditProfileDataFor where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.FormPart        (FormDF, fieldset, ol, li, multiFormPart)
import Clckwrks.ProfileData.Acid (GetProfileData(..), SetProfileData(..), profileDataErrorStr)
import Data.Set                 as Set
import Data.Text                (Text)
import qualified Data.Text      as Text
import Happstack.Auth           (UserId)
import Text.Digestive           (Transformer, (++>), transform, transformEitherM)
import Text.Digestive.HSP.Html4 (inputCheckboxes, inputText, label, submit)

editProfileDataForPage :: ProfileDataURL -> UserId -> Clck ProfileDataURL Response
editProfileDataForPage here uid =
    do mpd <- query (GetProfileData uid)
       case mpd of
         Nothing ->
             do notFound ()
                template "Edit Profile Data" () $
                         <p>No profile data for <% show uid %>.</p>
         (Just pd) ->
             do action <- showURL here
                template "Edit Profile Data" () $
                 <% multiFormPart "epd" action updated Nothing (profileDataFormlet pd) %>

    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

profileDataFormlet :: ProfileData -> FormDF (Clck ProfileDataURL) ()
profileDataFormlet pd@ProfileData{..} =
    (fieldset $
       ol $ (,,) <$> ((li $ label "username:")            ++>
                        (li $ inputText (Just username)))
                <*> ((li $ label "email (optional):")     ++>
                        (li $ inputText email))
                <*> ((li $ label "roles:")                ++>
                        (li $ inputCheckboxes False (Set.toList roles) [ (r, show r) | r <- [minBound .. maxBound]]))

                <*  submit "update")
    `transform` updateProfileData
    where
      updateProfileData :: (Functor m, MonadIO m) => Transformer (ClckT url m) String (Text, Text, [Role]) ()
      updateProfileData =
          transformEitherM $ \(usrnm, eml, roles') ->
              if Text.null usrnm
                 then do return (Left $ "Username can not be empty.")
                 else do let newPd = pd { username = usrnm
                                        , email    = if Text.null eml then Nothing else (Just eml)
                                        , roles    = Set.fromList roles'
                                        }
                         merr <- update (SetProfileData newPd)
                         case merr of
                           Nothing    -> return $ Right ()
                           (Just err) -> return $ Left (profileDataErrorStr err)

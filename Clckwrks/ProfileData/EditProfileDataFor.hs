{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.ProfileData.EditProfileDataFor where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.ProfileData.Acid (GetProfileData(..), SetProfileData(..))
import Data.Maybe               (fromMaybe)
import Data.Set                 as Set
import Data.Text                (Text, pack)
import qualified Data.Text      as Text
import Data.UserId              (UserId)
import HSP.XMLGenerator
import HSP.XML
import Text.Reform              ((++>), transformEitherM)
import Text.Reform.Happstack    (reform)
import Text.Reform.HSP.Text     (inputCheckboxes, inputText, labelText, inputSubmit, fieldset, ol, li, form)

editProfileDataForPage :: ProfileDataURL -> UserId -> Clck ProfileDataURL Response
editProfileDataForPage here uid =
    do pd <- query (GetProfileData uid)
       action <- showURL here
       template "Edit Profile Data" () $
         <% reform (form action) "epd" updated Nothing (profileDataFormlet pd) %>

    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

profileDataFormlet :: ProfileData -> ClckForm ProfileDataURL ()
profileDataFormlet pd@ProfileData{..} =
    (fieldset $
      ol $
       ((li $ labelText "roles:")            ++> (li $ inputCheckboxes [ (r, show r) | r <- [minBound .. maxBound]] (\r -> Set.member r roles))
             <*  inputSubmit (pack "update")
       )
    ) `transformEitherM` updateProfileData
    where
      updateProfileData :: [Role] -> Clck ProfileDataURL (Either ClckFormError ())
      updateProfileData roles' =
        do let newPd = pd { roles    = Set.fromList roles'
                          }
           update (SetProfileData newPd)
           pure (Right ())

{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.ProfileData.EditProfileData where

import Clckwrks
import Clckwrks.Admin.Template     (template)
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..), profileDataErrorStr)
import Data.Text                   (pack)
import qualified Data.Text         as Text
import Data.Text.Lazy              (Text)
import Data.Maybe                  (fromMaybe)
import Happstack.Authenticate.Core (UserId)
import Text.Reform                 ((++>), mapView, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

editProfileDataPage :: ProfileDataURL -> Clck ProfileDataURL Response
editProfileDataPage here =
    do mUid <- getUserId
       case mUid of
         Nothing -> internalServerError $ toResponse $ ("Unable to retrieve your userid" :: Text)
         (Just uid) ->
             do pd <- query (GetProfileData uid)
                action <- showURL here
                template "Edit Profile Data" () $
                  <%>
                    <% reform (form action) "epd" updated Nothing (profileDataFormlet pd) %>
                    <up-change-password />
                  </%>
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

profileDataFormlet :: ProfileData -> ClckForm ProfileDataURL ()
profileDataFormlet pd@ProfileData{..} =
    divHorizontal $
     errorList ++>
          ((,) <$> (divControlGroup (label' "Username" ++> (divControls (inputText username))))
               <*> (divControlGroup (label'" Email (optional)" ++> (divControls (inputText (fromMaybe Text.empty email)))))
               <*  (divControlGroup (divControls (inputSubmit (pack "Update") `setAttrs` (("class" := "btn") :: Attr Text Text)))))
    `transformEitherM` updateProfileData
    where
      label' :: Text -> ClckForm ProfileDataURL ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
      divHorizontal   = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls     = mapView (\xml -> [<div class="controls"><% xml %></div>])

      updateProfileData :: (Text.Text, Text.Text) -> Clck ProfileDataURL (Either ClckFormError ())
      updateProfileData (usrnm, eml) =
          do let newPd = pd { username = usrnm
                            , email    = if Text.null eml then Nothing else (Just eml)
                            }
             merr <- update (SetProfileData newPd)
             case merr of
               Nothing    -> return $ Right ()
               (Just err) -> return $ Left (PDE err)

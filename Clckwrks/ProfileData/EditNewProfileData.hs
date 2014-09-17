{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.ProfileData.EditNewProfileData where

import Clckwrks
import Clckwrks.Monad              (getRedirectCookie)
import Clckwrks.Admin.Template     (emptyTemplate)
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..), profileDataErrorStr)
import Clckwrks.ProfileData.EditProfileData(profileDataFormlet)
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

editNewProfileDataPage :: ProfileDataURL -> Clck ProfileDataURL Response
editNewProfileDataPage here =
    do mUid <- getUserId
       case mUid of
         Nothing -> internalServerError $ toResponse $ ("Unable to retrieve your userid" :: Text)
         (Just uid) ->
             do pd <- query (GetProfileData uid)
                action <- showURL here
                emptyTemplate "Edit Profile Data" () $
                  <%>
                    <% reform (form action) "epd" updated Nothing (profileDataFormlet pd) %>
                  </%>
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do mrc <- getRedirectCookie
             case mrc of
               Nothing ->
                   do mlr <- query GetLoginRedirect
                      case mlr of
                        Nothing -> seeOtherURL EditProfileData
                        (Just lr) -> seeOther lr (toResponse ())
               (Just u) ->
                   seeOther u (toResponse ())




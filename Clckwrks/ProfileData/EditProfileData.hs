{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.ProfileData.EditProfileData where

import Clckwrks
import Clckwrks.Monad              (plugins)
import Clckwrks.Admin.Template     (template)
import Clckwrks.Authenticate.Plugin (AcidStateAuthenticate(..), authenticatePlugin)
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..))
import Control.Monad.State         (get)
import Control.Monad.Trans         (liftIO)
import qualified Data.Acid         as Acid
import Data.Text                   (pack)
import qualified Data.Text         as Text
import Data.Text.Lazy              (Text)
import Data.Maybe                  (fromMaybe)
import Data.UserId                 (UserId)
import Happstack.Authenticate.Core (Email(..), User(..), GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), mapView, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

editProfileDataPage :: ProfileDataURL -> Clck ProfileDataURL Response
editProfileDataPage here =
    do mUid <- getUserId
       case mUid of
         Nothing -> internalServerError $ toResponse $ ("Unable to retrieve your userid" :: Text)
         (Just uid) ->
             do -- pd <- query (GetProfileData uid)
                p <- plugins <$> get
                ~(Just (AcidStateAuthenticate authenticateState)) <- getPluginState p (pluginName authenticatePlugin)
                ~(Just user) <- liftIO $ Acid.query authenticateState (GetUserByUserId uid)
                action <- showURL here
                template "Edit Profile Data" () $ [hsx|
                  <%>
                    <% reform (form action) "epd" updated Nothing (emailFormlet user) %>
                    <up-change-password />
                  </%> |]
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

emailFormlet :: User -> ClckForm ProfileDataURL ()
emailFormlet u@User{..} =
    divHorizontal $
     errorList ++>
          ((divControlGroup (label'" Email" ++> (divControls (inputText (maybe Text.empty _unEmail _email)))))
               <*  (divControlGroup (divControls (inputSubmit (pack "Update") `setAttrs` (("class" := "btn") :: Attr Text Text)))))
    `transformEitherM` updateEmail
  where
    label' :: Text -> ClckForm ProfileDataURL ()
    label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
    divHorizontal   = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
    divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
    divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])

    updateEmail :: Text.Text -> Clck ProfileDataURL (Either ClckFormError ())
    updateEmail eml =
      do let user = u { _email    = if Text.null eml then Nothing else (Just (Email eml))
                      }
         p <- plugins <$> get
         ~(Just (AcidStateAuthenticate authenticateState)) <- getPluginState p (pluginName authenticatePlugin)
         liftIO $ Acid.update authenticateState  (UpdateUser user)
         pure $ Right ()

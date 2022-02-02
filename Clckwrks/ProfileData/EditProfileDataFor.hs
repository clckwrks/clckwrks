{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.ProfileData.EditProfileDataFor where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.ProfileData.Acid (GetProfileData(..), SetProfileData(..))
import Clckwrks.Authenticate.Monad (AcidStateAuthenticate(..))
import Clckwrks.Authenticate.URL   (AuthURL(ResetPassword))
import Control.Monad.State         (get)
import Data.Maybe               (fromMaybe)
import Data.Set                 as Set
import Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text      as Text
import Data.UserId              (UserId)

import Language.Haskell.HSX.QQ  (hsx)
import Happstack.Authenticate.Core (Email(..), GetUserByUserId(..), User(..), UserId(..), Username(..))
import Happstack.Authenticate.Password.Core (SetPassword(..), mkHashedPass, resetTokenForUserId)
import HSP.XMLGenerator
import HSP.XML
import System.FilePath             ((</>))
import Text.Reform              ((++>), mapView, transformEitherM)
import Text.Reform.Happstack    (reform)
import Text.Reform.HSP.Text     (inputCheckboxes, inputPassword, inputText, labelText, inputSubmit, fieldset, ol, li, form, setAttrs)
import Web.Plugins.Core         (getConfig, getPluginState, getPluginRouteFn)

editProfileDataForPage :: ProfileDataURL -> UserId -> Clck ProfileDataURL Response
editProfileDataForPage here uid =
    do pd <- query (GetProfileData uid)
       mu  <- query (GetUserByUserId uid)
       case mu of
         Nothing ->
           template "Edit Profile Data" () $
             [hsx|
               <div>Invalid UserId <% show uid  %></div>
             |]
         (Just u) ->
           do action <- showURL here
              template "Edit Profile Data" () $ [hsx|
                <div>
                 <h2>User Info</h2>
                 <dl>
                  <dt>UserId</dt>  <dd><% show $ _unUserId $ _userId u %></dd>
                  <dt>Username</dt><dd><% _unUsername $ _username u %></dd>
                  <dt>Email</dt>   <dd><% maybe Text.empty _unEmail  (_email u) %></dd>
                 </dl>
                 <h2>Roles</h2>
                 <% reform (form action) "epd" updated Nothing (profileDataFormlet pd) %>
                 <h2>Update User's Password</h2>
                 <% reform (form action) "pf"  updated Nothing (passwordForFormlet uid) %>
                 <h2>Generate Password Reset Link</h2>
                 <% reform (form action) "prl"  (generateResetLink uid) Nothing generateResetLinkFormlet %>
               </div>
               |]
    where
      updated :: () -> Clck ProfileDataURL Response
      updated () =
          do seeOtherURL here

      generateResetLink :: UserId -> Maybe Text.Text -> Clck ProfileDataURL Response
      generateResetLink uid _ =
        do p <- plugins <$> get
           ~(Just (AcidStateAuthenticate authenticateState passwordState)) <- getPluginState p "authenticate"
           ~(Just authShowURL) <- getPluginRouteFn p "authenticate"
           cc <- getConfig p
           let basePath = maybe "_state" (\top -> top </> "_state") (clckTopDir cc)
               baseUri = case calcTLSBaseURI cc of
                 Nothing  -> calcBaseURI cc
                 (Just b) -> b

               resetLink = authShowURL ResetPassword [] <> "/#"
           eResetTokenLink <- liftIO $ resetTokenForUserId resetLink authenticateState passwordState uid
           case eResetTokenLink of
             (Left e) -> template "Reset Password Link" () $
                [hsx|
                 <div>
                  <h2>Reset Password Link Error</h2>
                  <% show e %>
                 </div>
                |]
             (Right lnk) ->
               template "Reset Password Link" () $
                [hsx|
                 <div>
                  <h2>Reset Password Link</h2>
                  <p>Share this link with the user</p>
                  <% lnk %>
                 </div>
                |]

generateResetLinkFormlet :: ClckForm ProfileDataURL (Maybe Text.Text)
generateResetLinkFormlet =
  do (fieldset $
      (divControlGroup $ divControls $ inputSubmit "Change Password"  `setAttrs` [("class" := "btn") :: Attr Text Text]))
   where
     divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
     divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])


passwordForFormlet :: UserId -> ClckForm ProfileDataURL ()
passwordForFormlet userid =
    (fieldset $
      (divControlGroup $
        (divControls (label' "new password" ++> inputPassword))
        )
      <* (divControlGroup $ divControls $ inputSubmit "Change Password"  `setAttrs` [("class" := "btn") :: Attr Text Text])
      ) `transformEitherM` updatePassword
    where
      label' :: Text -> ClckForm ProfileDataURL ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
--       divHorizontal   = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
      divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
      divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])

      updatePassword :: Text.Text -> Clck ProfileDataURL (Either ClckFormError ())
      updatePassword newPw
        | Text.null newPw = pure (Right ())
        | otherwise =
            do hp <- mkHashedPass newPw
               update (SetPassword userid hp)
               pure (Right ())

profileDataFormlet :: ProfileData -> ClckForm ProfileDataURL ()
profileDataFormlet pd@ProfileData{..} =
    (fieldset $
      (divControlGroup $
        (divControls (inputCheckboxes [ (r, show r) | r <- [minBound .. maxBound]] (\r -> Set.member r roles)) `setAttrs` (("class" := "form-check") :: Attr Text Text)))
      <* (divControlGroup $ divControls $ inputSubmit "Update Roles"  `setAttrs` [("class" := "btn") :: Attr Text Text])
    ) `transformEitherM` updateProfileData
    where
      label' :: Text -> ClckForm ProfileDataURL ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
--       divHorizontal   = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
      divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
      divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])

      updateProfileData :: [Role] -> Clck ProfileDataURL (Either ClckFormError ())
      updateProfileData roles' =
        do let newPd = pd { roles    = Set.fromList roles'
                          }
           update (SetProfileData newPd)
           pure (Right ())

--       ((li $ labelText "roles:")            ++> ((li $ inputCheckboxes [ (r, show r) | r <- [minBound .. maxBound]] (\r -> Set.member r roles)) `setAttrs` (("class" := "form-check") :: Attr Text Text))

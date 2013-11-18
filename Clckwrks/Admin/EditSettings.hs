{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Admin.EditSettings where

import Clckwrks
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..))
import Clckwrks.Admin.Template   (template)
import Data.Maybe                (fromMaybe)
import Data.Text            (Text, pack, unpack)
import qualified Data.Text  as T

-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import HSP.Google.Analytics      (UACCT(..))
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text

editSettings :: ClckURL -> Clck ClckURL Response
editSettings here =
    do coreState <- query $ GetCoreState
       action <- showURL here
       template "Edit Settings" () $
                  <%>
                   <% reform (form action) "ss" updateSettings Nothing (editSettingsForm coreState) %>
                  </%>
    where
      updateSettings :: CoreState -> Clck ClckURL Response
      updateSettings coreState =
          do update (SetCoreState coreState)
             seeOtherURL here

editSettingsForm :: CoreState -> ClckForm ClckURL CoreState
editSettingsForm CoreState{..} =
    divHorizontal $
     fieldset $
       (CoreState <$>
           (divControlGroup $
             (labelText "site name"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty coreSiteName)) `transformEither` toMaybe)))

       <*> (divControlGroup $
             (label ("Google Analytics UACCT" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (unUACCT coreUACCT)) `transformEither` toMUACCT))

       <*> (divControlGroup $
             (labelText "/ redirects to"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty coreRootRedirect)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (labelText "after login redirect to"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty coreLoginRedirect)) `transformEither` toMaybe))
         <*
        (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])
    where
      divHorizontal   = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls     = mapView (\xml -> [<div class="controls"><% xml %></div>])

      unUACCT (Just (UACCT str)) = pack str
      unUACCT Nothing            = mempty

      toMUACCT :: T.Text -> Either ClckFormError (Maybe UACCT)
      toMUACCT str | T.null str   = Right $ Nothing
      toMUACCT str = Right $ Just (UACCT $ T.unpack str)

      toMaybe :: Text -> Either ClckFormError (Maybe Text)
      toMaybe txt =
          if T.null txt
             then Right $ Nothing
             else Right $ Just txt

{-
editUACCTForm :: Maybe UACCT -> ClckForm ClckURL (Maybe UACCT)
editUACCTForm muacct =
    divHorizontal $
     fieldset $
        (divControlGroup $
         (label ("Google Analytics UACCT" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
          (divControls (inputText (unUACCT muacct)) `transformEither` toMUACCT)) <*
        (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])
    where
      divHorizontal   = mapView (\xml -> [<div class="form-horizontal"><% xml %></div>])
      divControlGroup = mapView (\xml -> [<div class="control-group"><% xml %></div>])
      divControls     = mapView (\xml -> [<div class="controls"><% xml %></div>])
      unUACCT (Just (UACCT str)) = str
      unUACCT Nothing            = ""

      toMUACCT :: String -> Either ClckFormError (Maybe UACCT)
      toMUACCT []  = Right $ Nothing
      toMUACCT str = Right $ Just (UACCT str)
-}
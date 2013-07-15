{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Admin.EditSettings where

import Clckwrks
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..))
import Clckwrks.Admin.Template   (template)
import Data.Text.Lazy            (Text)
-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import HSP.Google.Analytics      (UACCT(..))
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.String

editSettings :: ClckURL -> Clck ClckURL Response
editSettings here =
    do muacct <- query $ GetUACCT
       action <- showURL here
       template "Edit Settings" () $
                  <%>
                   <% reform (form action) "ss" updateSettings Nothing (editSettingsForm muacct) %>
                  </%>
    where
      updateSettings :: Maybe UACCT -> Clck ClckURL Response
      updateSettings muacct =
          do update (SetUACCT muacct)
             seeOtherURL (Admin Console)

editSettingsForm :: Maybe UACCT -> ClckForm ClckURL (Maybe UACCT)
editSettingsForm muacct =
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



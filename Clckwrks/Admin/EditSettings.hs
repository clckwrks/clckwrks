{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditSettings where

import Clckwrks
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..))
import Clckwrks.Admin.Template  (template)
-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import HSP.Google.Analytics     (UACCT(..))
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
    fieldset $
     ol $
      ((li $ label "Google Analytics UACCT:") ++> (li $ (inputText (unUACCT muacct)) `transformEither` toMUACCT)) <*
      inputSubmit "update"
    where
      unUACCT (Just (UACCT str)) = str
      unUACCT Nothing            = ""

      toMUACCT :: String -> Either ClckFormError (Maybe UACCT)
      toMUACCT []  = Right $ Nothing
      toMUACCT str = Right $ Just (UACCT str)

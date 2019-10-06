{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Admin.SystemEmails where

import Clckwrks
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..))
import Clckwrks.Admin.Template   (template)
import Control.Lens              ((.~), (&))
import Data.Maybe                (maybe, fromMaybe)
import Data.Text            (Text, pack, unpack)
import qualified Data.Text  as T

-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import Happstack.Authenticate.Core (Email(..), SimpleAddress(..))
import HSP.Google.Analytics      (UACCT(..))
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Language.Haskell.HSX.QQ   (hsx)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text

systemEmailsPage :: ClckURL -> Clck ClckURL Response
systemEmailsPage here =
    do coreState <- query $ GetCoreState
       action <- showURL here
       template "Edit Settings" () $ [hsx|
                  <%>
                   <% reform (form action) "ss" updateSettings Nothing (editSettingsForm coreState) %>
                  </%> |]
    where
      updateSettings :: CoreState -> Clck ClckURL Response
      updateSettings coreState =
          do update (SetCoreState coreState)
             seeOtherURL here


editSettingsForm :: CoreState -> ClckForm ClckURL CoreState
editSettingsForm c@CoreState{..} =
    divHorizontal $
     fieldset $
       (modifyCoreState <$>
           (divControlGroup $
             (labelText "From: address"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (maybe mempty (_unEmail . _saEmail) _coreFromAddress)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (label ("From: name" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (maybe mempty (fromMaybe mempty . _saName) _coreFromAddress)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (label ("Reply-to: address" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (maybe mempty (_unEmail . _saEmail) _coreReplyToAddress)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (label ("Reply-to: name" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (maybe mempty (fromMaybe mempty . _saName) _coreReplyToAddress)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (labelText "sendmail path"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (maybe mempty T.pack _coreSendmailPath)) `transformEither` toMaybe)))

         <*
        (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])
    where
      divHorizontal   = mapView (\xml -> [[hsx| <div class="form-horizontal"><% xml %></div>|]])
      divControlGroup = mapView (\xml -> [[hsx| <div class="control-group"><% xml %></div>|]])
      divControls     = mapView (\xml -> [[hsx| <div class="controls"><% xml %></div>|]])

      toMaybe :: Text -> Either ClckFormError (Maybe Text)
      toMaybe txt =
          if T.null txt
             then Right $ Nothing
             else Right $ Just txt
      modifyCoreState mFromAddress mFromName mReplyToAddress mReplyToName mSendmailPath =
        c & coreFromAddress .~
             case mFromAddress of
               Nothing -> Nothing
               (Just addr) -> Just (SimpleAddress mFromName (Email addr))
          & coreReplyToAddress .~
             case mReplyToAddress of
               Nothing -> Nothing
               (Just addr) -> Just (SimpleAddress mReplyToName (Email addr))
          & coreSendmailPath .~ (T.unpack <$> mSendmailPath)

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

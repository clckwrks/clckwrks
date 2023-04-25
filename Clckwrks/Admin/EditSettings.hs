{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Clckwrks.Admin.EditSettings where

import Clckwrks                  hiding (transform)
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..), coreSiteName, coreUACCT, coreRootRedirect, coreLoginRedirect, coreBackToSiteRedirect)
import Clckwrks.Admin.Template   (template)
import Control.Lens              ((&), (.~))
import Data.Maybe                (fromMaybe)
import Data.Text            (Text, pack, unpack)
import qualified Data.Text  as T

-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import HSP.Google.Analytics      (UACCT(..))
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Numeric                   (readDec)
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
editSettingsForm c@CoreState{..} =
    divHorizontal $
     fieldset $
       (modifyCoreState <$>
           (divControlGroup $
             (labelText "site name"                    `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty _coreSiteName)) `transformEither` toMaybe)))

       <*> (divControlGroup $
             (label ("Google Analytics UACCT" :: Text) `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (unUACCT _coreUACCT)) `transformEither` toMUACCT))

       <*> (divControlGroup $
             (labelText "/ redirects to"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty _coreRootRedirect)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (labelText "Back To Site location"         `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText _coreBackToSiteRedirect)))

       <*> (divControlGroup $
             (labelText "after login redirect to"        `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty _coreLoginRedirect)) `transformEither` toMaybe))

       <*> (divControlGroup $
             (labelText "after signup redirect to"        `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
             (divControls (inputText (fromMaybe mempty _coreSignupRedirect)) `transformEither` toMaybe))

       <*> bodyPolicyForm
       <*
        (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])
    where
      bodyPolicyForm =
        let (tmpPath, mDisk, mRam, mHeader) = _coreBodyPolicy in
        (,,,) <$> (divControlGroup $
                   (labelText "temporary directory for uploads" `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
                   (divControls (inputText (T.pack tmpPath)) `transformEitherM` toFilePath))
              <*> (divControlGroup $
                   (labelText "maximum size for file uploads" `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
                   (divControls (inputText (T.pack $ show $ mDisk)) `transform` (decimalText InvalidDecimal)))
              <*> (divControlGroup $
                   (labelText "maximum size for non-file values" `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
                   (divControls (inputText (T.pack $ show $ mRam)) `transform` (decimalText InvalidDecimal)))
              <*> (divControlGroup $
                   (labelText "maximum size for overhead of headers in multipart/form-data" `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
                   (divControls (inputText (T.pack $ show $ mHeader)) `transform` (decimalText InvalidDecimal)))


      -- | read an unsigned number in decimal notation
      decimalText :: (Monad m, Eq i, Num i) =>
                 (Text -> error) -- ^ create an error message ('String' is the value that did not parse)
              -> Proof m error Decimal Text i
      decimalText mkError = Proof Decimal (return . toDecimal)
        where
          toDecimal txt =
            case readDec (T.unpack txt) of
              [(d,[])] -> (Right d)
              _        -> (Left $ mkError txt)

      -- | FIXME: should perhaps check that the path exists and is writable?
      toFilePath :: (MonadIO m) => Text -> m (Either ClckFormError FilePath)
      toFilePath t = pure $ Right (T.unpack t)

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

      modifyCoreState sn ua rr bts lr sr bp =
        c & coreSiteName       .~ sn
          & coreUACCT          .~ ua
          & coreRootRedirect   .~ rr
          & coreBackToSiteRedirect .~ bts
          & coreLoginRedirect  .~ lr
          & coreSignupRedirect .~ sr
          & coreBodyPolicy     .~ bp

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

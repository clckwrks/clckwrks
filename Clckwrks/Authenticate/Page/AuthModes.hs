{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.AuthModes where

import Clckwrks.Acid             (GetEnableOpenId(..), SetEnableOpenId(..))
import Clckwrks.Admin.Template   (template)
import Clckwrks.Authenticate.URL (AuthURL(..))
import Clckwrks.Monad
import Clckwrks.URL              (ClckURL)
import Control.Lens              ((.~), (&))
import Data.Maybe                (maybe, fromMaybe)
import Data.Text.Lazy            (Text)
import qualified Data.Text       as T
import Happstack.Server          (Response, ServerPartT, ok, toResponse)
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Language.Haskell.HSX.QQ   (hsx)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Web.Routes                (showURL)
import Web.Routes.Happstack      (seeOtherURL)

authModesPage :: AuthURL -> Clck AuthURL Response
authModesPage here =
  do enableOpenId <- query $ GetEnableOpenId
     action <- showURL here
     template "Authentication Modes" () $
       [hsx|
           <%>
            <% reform (form action) "am" updateAuthModes Nothing (authModesForm enableOpenId) %>
           </%>
           |]
       where
         updateAuthModes :: Bool -> Clck AuthURL Response
         updateAuthModes b =
           do update (SetEnableOpenId b)
              seeOtherURL here

authModesForm :: Bool -> ClckForm AuthURL Bool
authModesForm b =
  divHorizontal $
     fieldset $
       (divControlGroup $
          (labelText "Enable OpenId"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
            (divControls (inputCheckbox b)))
       <* (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])

  where
    label' :: Text -> ClckForm AuthURL ()
    label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
    divHorizontal   = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
    divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
    divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])


--             (divControls (inputText (maybe mempty (_unEmail . _saEmail) _coreFromAddress)) `transformEither` toMaybe))

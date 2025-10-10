{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Authenticate.Page.Turnstile where

import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))

import Clckwrks.Acid             (GetEnableOpenId(..), SetEnableOpenId(..))
import Clckwrks.Admin.Template   (template)
import Clckwrks.Authenticate.URL (AuthURL(..))
import Clckwrks.Monad            (Clck, ClckForm, plugins)
import Clckwrks.URL              (ClckURL)
import Control.Monad.State         (get)
import Control.Lens              ((.~), (&))
import Control.Monad.Trans      (liftIO)
import Data.Acid                 (AcidState)
import qualified Data.Acid         as Acid
import Data.Maybe                (maybe, fromMaybe)
import Data.Text.Lazy            (Text)
import qualified Data.Text       as T
import Happstack.Authenticate.Handlers (AuthenticateState(..), Turnstile(..), GetTurnstile(..), SetTurnstile(..))
import Happstack.Server          (Response, ServerPartT, ok, toResponse)
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Language.Haskell.HSX.QQ   (hsx)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import Web.Plugins.Core            (Plugin(..), getPluginState)
import Web.Routes                (showURL)
import Web.Routes.Happstack      (seeOtherURL)


turnstileConfig :: AuthURL -> Clck AuthURL Response
turnstileConfig here =
  do action <- showURL here
     p <- plugins <$> get
     ~(Just aps) <- getPluginState p "authenticate"
     mTurnstileData <- liftIO $ Acid.query (acidStateAuthenticate aps) GetTurnstile
     template "Turnstile Config" () $
       [hsx|
           <%>
            <% reform (form action) "am" (updateTurnstile (acidStateAuthenticate aps)) Nothing (turnstileForm mTurnstileData)  %>
           </%>
           |]
         where
           updateTurnstile :: AcidState AuthenticateState -> Maybe Turnstile -> Clck AuthURL Response
           updateTurnstile authState t =
             do liftIO $ Acid.update authState  (SetTurnstile t)
                seeOtherURL here

turnstileForm :: Maybe Turnstile -> ClckForm AuthURL (Maybe Turnstile)
turnstileForm mTurn =
  let (siteKey, siteSecret) =
        case mTurn of
          Nothing -> ("", "")
          (Just (Turnstile key secret)) -> (key, secret)
  in
    (fmap mkTurnstile $
    divHorizontal $
     fieldset $
        ((,) <$>
          (divControlGroup $
           ((labelText "Turnstile Site Key"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
            (divControls (inputText siteKey))))
            <*>
          (divControlGroup $
           ((labelText "Turnstile Site Secret"               `setAttrs` [("class":="control-label") :: Attr Text Text]) ++>
            (divControls (inputText siteSecret))))))
           <* (divControlGroup $ divControls $ inputSubmit "Update" `setAttrs` [("class" := "btn") :: Attr Text Text])
  where
    mkTurnstile :: (T.Text, T.Text) -> Maybe Turnstile
    mkTurnstile (siteKey, siteSecret) = Just (Turnstile siteKey siteSecret)

    label' :: Text -> ClckForm AuthURL ()
    label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])
    divHorizontal   = mapView (\xml -> [[hsx|<div class="form-horizontal"><% xml %></div>|]])
    divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
    divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])

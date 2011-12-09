{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.EditPage where

import Admin.URL
import Admin.Template
import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import FormPart (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Page.Acid
import Text.Digestive
import Text.Digestive.HSP.Html4 hiding (inputTextArea)

editPage :: ClckURL -> PageId -> Clck ClckURL Response
editPage here pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found" ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                template "edit page" () $ 
                  <%> 
                   <% multiFormPart "ep" action updatePage Nothing (pageFormlet page) %>
                  </%>
    where
      updatePage :: Page -> Clck ClckURL Response
      updatePage page =
          do update (UpdatePage page)
             seeOtherURL (ViewPage (pageId page)) 

pageFormlet :: Page -> FormDF (Clck ClckURL) Page
pageFormlet page =
    (fieldset $
       ol $ (,,) <$> (li $ inputCheckBox hsColour <++ label "Highlight Haskell code with HsColour")
                 <*> ((li $ label "title:") ++> (li $ inputText (Just (pageTitle page)) `setAttrs` ("size" := "80")))
                 <*> ((li $ label "body:") ++> (li $ inputTextArea (Just 80) (Just 25) (Just (markup (pageSrc page)))))
                 <*  submit "update")
    `transform` (transformEitherM toPage)
    where
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) => (Bool, Text, Text) -> m (Either e Page)
      toPage (haskell, ttl, bdy) =
          do now <- liftIO $ getCurrentTime
             return $ Right $ 
               Page { pageId    = pageId page
                    , pageTitle = ttl
                    , pageSrc   = Markup { preProcessors =  (if haskell then ([ HsColour ] ++) else id) [ Markdown ]
                                         , markup = bdy
                                         }
                    , pageExcerpt = Nothing
                    , pageDate    = Just now
                    , pageStatus  = Published
                    }

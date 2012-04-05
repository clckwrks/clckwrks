{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditPage where

import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks
import Clckwrks.Admin.Template (template)
import Clckwrks.FormPart       (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Data.Text               (Text)
import Data.Time.Clock         (getCurrentTime)
import Text.Digestive          ((<++), (++>), transform, transformEitherM)
import Text.Digestive.HSP.Html4 (inputCheckBox, inputSelect, inputText, label, setAttrs, submit)

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
       ol $ (,,,) <$> (li $ inputCheckBox hsColour <++ label "Highlight Haskell code with HsColour")
                  <*> ((li $ label "kind:")   ++> (li $ inputSelect (pageKind page) [(PlainPage, "page"), (Post, "post")]))
                  <*> ((li $ label "title:") ++> (li $ inputText (Just (pageTitle page)) `setAttrs` ("size" := "80")))
                  <*> ((li $ label "body:")  ++> (li $ inputTextArea (Just 80) (Just 25) (Just (markup (pageSrc page)))))
                  <*  submit "update")
    `transform` (transformEitherM toPage)
    where
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) => (Bool, PageKind, Text, Text) -> m (Either e Page)
      toPage (haskell, kind, ttl, bdy) =
          do now <- liftIO $ getCurrentTime
             return $ Right $
               Page { pageId    = pageId page
                    , pageTitle = ttl
                    , pageSrc   = Markup { preProcessors =  (if haskell then ([ HsColour ] ++) else id) [ Markdown ]
                                         , trust = Trusted
                                         , markup = bdy
                                         }
                    , pageExcerpt = Nothing
                    , pageDate    = Just now
                    , pageStatus  = Published
                    , pageKind    = kind
                    }

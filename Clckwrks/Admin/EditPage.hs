{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditPage
    ( editPage
    ) where

import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks
import Clckwrks.Admin.Template (template)
import Clckwrks.Monad          (ClckFormError)
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Data.Maybe              (isJust, maybe)
import Data.Text               (Text, pack)
import qualified Data.Text     as Text
import Data.Time.Clock         (getCurrentTime)
import Text.Reform             ((<++), (++>), transformEitherM)
import Text.Reform.Happstack   (reform)
import Text.Reform.HSP.Text    (form, inputCheckbox, inputText, label, inputSubmit, select, textarea, fieldset, ol, li, setAttrs)

data AfterSaveAction
    = EditSomeMore
    | VisitPage
    | ShowPreview

editPage :: ClckURL -> PageId -> Clck ClckURL Response
editPage here pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found: " ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                template "edit page" () $
                  <%>
                   <% reform (form action) "ep" updatePage Nothing (pageFormlet page) %>
                  </%>
    where
      updatePage :: (Page, AfterSaveAction) -> Clck ClckURL Response
      updatePage (page, afterSaveAction) =
          do update (UpdatePage page)
             case afterSaveAction of
               EditSomeMore -> seeOtherURL (Admin $ EditPage    (pageId page))
               VisitPage    -> seeOtherURL (ViewPageSlug (pageId page) (toSlug (pageTitle page) (pageSlug page)))
               ShowPreview  -> seeOtherURL (Admin $ PreviewPage (pageId page))


pageFormlet :: Page -> ClckForm ClckURL (Page, AfterSaveAction)
pageFormlet page =
    (fieldset $
       ol $ (,,,,,,,)
              <$> (li $ inputCheckbox hsColour <++ label "Highlight Haskell code with HsColour")
              <*> ((li $ label "kind:")  ++> (li $ select [(PlainPage, "page"), (Post, "post")] (== (pageKind page))))
              <*> ((li $ label "title:") ++> (li $ inputText (pageTitle page) `setAttrs` ("size" := "80") ))
              <*> ((li $ label "slug (optional):") ++> (li $ inputText (maybe Text.empty unSlug $ pageSlug page) `setAttrs` ("size" := "80") ))
              <*> ((li $ label "body:")  ++> (li $ textarea 80 25 (markup (pageSrc page))))
              <*> inputSubmit (pack "save")
              <*> inputSubmit (pack "preview")
              <*> newPublishStatus (pageStatus page)
    ) `transformEitherM` toPage
    where
      newPublishStatus :: PublishStatus -> ClckForm ClckURL (Maybe PublishStatus)
      newPublishStatus Published = fmap (const Draft)     <$> inputSubmit (pack "save & unpublish")
      newPublishStatus _         = fmap (const Published) <$> inputSubmit (pack "save & publish")
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) => (Bool, PageKind, Text, Text, Text, Maybe Text, Maybe Text, Maybe PublishStatus) -> m (Either ClckFormError (Page, AfterSaveAction))
      toPage (haskell, kind, ttl, slug, bdy, msave, mpreview, mpagestatus) =
          do now <- liftIO $ getCurrentTime
             return $ Right $
               ( Page { pageId      = pageId page
                      , pageAuthor  = pageAuthor page
                      , pageTitle   = ttl
                      , pageSlug    = if Text.null slug then Nothing else Just (slugify slug)
                      , pageSrc     = Markup { preProcessors =  (if haskell then ([ HsColour ] ++) else id) [ Markdown ]
                                             , trust = Trusted
                                             , markup = bdy
                                             }
                      , pageExcerpt = Nothing
                      , pageDate    = pageDate page
                      , pageUpdated = now
                      , pageStatus  = case mpagestatus of
                                        (Just newStatus) -> newStatus
                                        Nothing          -> pageStatus page
                      , pageKind    = kind
                      , pageUUID    = pageUUID page
                      }
               , if isJust mpreview
                 then ShowPreview
                 else case mpagestatus of
                       (Just Published) -> VisitPage
                       _                -> EditSomeMore
               )

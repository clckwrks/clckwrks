{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditPage where

import Control.Applicative ((<$>), (<*>), (<*))
import Clckwrks
import Clckwrks.Admin.Template (template)
import Clckwrks.Monad          (ClckFormError)
import Clckwrks.Page.Acid      (Markup(..), Page(..), PageKind(..), PublishStatus(..), PreProcessor(..), PageById(..), UpdatePage(..))
import Data.Text               (Text, pack)
import Data.Time.Clock         (getCurrentTime)
import Text.Reform             ((<++), (++>), transformEitherM)
import Text.Reform.Happstack   (reform)
import Text.Reform.HSP.Text    (form, inputCheckbox, inputText, label, inputSubmit, select, textarea, fieldset, ol, li, setAttrs)

editPage :: ClckURL -> PageId -> Clck ClckURL Response
editPage here pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found" ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                template "edit page" () $
                  <%>
                   <% reform (form action) "ep" updatePage Nothing (pageFormlet page) %>
                  </%>
    where
      updatePage :: Page -> Clck ClckURL Response
      updatePage page =
          do update (UpdatePage page)
             seeOtherURL (ViewPage (pageId page))

pageFormlet :: Page -> ClckForm ClckURL Page
pageFormlet page =
    (fieldset $
       ol $ (,,,) <$> (li $ inputCheckbox hsColour <++ label "Highlight Haskell code with HsColour")
                  <*> ((li $ label "kind:")  ++> (li $ select [(PlainPage, "page"), (Post, "post")] (== (pageKind page))))
                  <*> ((li $ label "title:") ++> (li $ inputText (pageTitle page) `setAttrs` ("size" := "80") ))
                  <*> ((li $ label "body:")  ++> (li $ textarea 80 25 (markup (pageSrc page))))
                  <*  inputSubmit (pack "update"))
    `transformEitherM` toPage
    where
      hsColour = HsColour `elem` (preProcessors $ pageSrc page)
      toPage :: (MonadIO m) => (Bool, PageKind, Text, Text) -> m (Either ClckFormError Page)
      toPage (haskell, kind, ttl, bdy) =
          do now <- liftIO $ getCurrentTime
             return $ Right $
               Page { pageId      = pageId page
                    , pageAuthor  = pageAuthor page
                    , pageTitle   = ttl
                    , pageSrc     = Markup { preProcessors =  (if haskell then ([ HsColour ] ++) else id) [ Markdown ]
                                           , trust = Trusted
                                           , markup = bdy
                                           }
                    , pageExcerpt = Nothing
                    , pageDate    = pageDate page
                    , pageUpdated = now
                    , pageStatus  = Published
                    , pageKind    = kind
                    , pageUUID    = pageUUID page
                    }

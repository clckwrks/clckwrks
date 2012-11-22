{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.API
    ( PageId(..)
    , getPage
    , getPageId
    , getPageTitle
    , getPageTitleSlug
    , getPageContent
    , getPagesSummary
    , getPageSummary
    , getPageMenu
    , getPosts
    , extractExcerpt
    , getBlogTitle
    , googleAnalytics
    ) where

import Clckwrks.Acid
import Clckwrks.Monad
import Clckwrks.Page.Acid
import Clckwrks.URL
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import Data.Text (Text, empty)
import qualified Data.Text as Text
import Clckwrks.Page.Types (toSlug)
import Happstack.Server
import HSP hiding (escape)
import HSP.Google.Analytics (analyticsAsync)
import Text.HTML.TagSoup

getPage :: Clck url Page
getPage =
    do ClckState{..} <- get
       mPage <- query (PageById currentPage)
       case mPage of
         Nothing -> escape $ internalServerError $ toResponse ("getPage: invalid PageId " ++ show (unPageId currentPage))
         (Just p) -> return p

getPageId :: Clck url PageId
getPageId = currentPage <$> get

getPageTitle :: Clck url Text
getPageTitle = pageTitle <$> getPage

getPageTitleSlug :: Clck url (Text, Maybe Slug)
getPageTitleSlug =
    do p <- getPage
       return (pageTitle p, pageSlug p)

getPageContent :: Clck url Content
getPageContent =
    do mrkup <- pageSrc <$> getPage
       markupToContent mrkup

getPagesSummary :: Clck url [(PageId, Text, Maybe Slug)]
getPagesSummary = query PagesSummary

getPageMenu :: GenXML (Clck ClckURL)
getPageMenu =
    do ps <- query PagesSummary
       case ps of
         [] -> <div>No pages found.</div>
         _ -> <ul class="page-menu">
                <% mapM (\(pid, ttl, slug) -> <li><a href=(ViewPageSlug pid (toSlug ttl slug)) title=ttl><% ttl %></a></li>) ps %>
              </ul>

getPageSummary :: PageId -> Clck url Content
getPageSummary pid =
    do mPage <- query (PageById pid)
       case mPage of
         Nothing ->
             return $ PlainText $ Text.pack $ "Invalid PageId " ++ (show $ unPageId pid)
         (Just pge) ->
             extractExcerpt pge

getBlogTitle :: Clck url Text
getBlogTitle = query GetBlogTitle

extractExcerpt :: (MonadIO m, Functor m, Happstack m) =>
                  Page
               -> ClckT url m Content
extractExcerpt Page{..} =
             case pageExcerpt of
               (Just excerpt) ->
                   markupToContent excerpt
               Nothing ->
                   do c <- markupToContent pageSrc
                      case c of
                        (TrustedHtml html) ->
                            let tags = parseTags html
                                paragraphs = sections (~== "<p>") tags
                                paragraph = case paragraphs of
                                              [] -> Text.pack "no summary available."
                                              (p:ps) -> renderTags $ takeThrough (not . isTagCloseName (Text.pack "p")) $ filter (not . isTagOpenName (Text.pack "img")) p
                            in return (TrustedHtml paragraph)
                        (PlainText text) ->
                               return (PlainText text)

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough f (p:ps)
    | f p = p : takeThrough f ps
    | otherwise = []

-- | get all posts, sorted reverse cronological
getPosts :: XMLGenT (Clck url) [Page]
getPosts = query AllPosts

-- | create a google analytics tracking code block
--
-- This will under two different conditions:
--
--  * the 'enableAnalytics' field in 'ClckState' is 'False'
--
--  * the 'uacct' field in 'PageState' is 'Nothing'
googleAnalytics :: XMLGenT (Clck url) XML
googleAnalytics =
    do enabled <- getEnableAnalytics
       case enabled of
         False -> return $ cdata ""
         True ->
             do muacct <- query GetUACCT
                case muacct of
                  Nothing -> return $ cdata ""
                  (Just uacct) ->
                      analyticsAsync uacct

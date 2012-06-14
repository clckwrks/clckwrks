{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Page.Atom where

import Control.Monad.Trans (liftIO)
import Clckwrks.Monad
import Clckwrks.Page.Acid
import Clckwrks.Page.Types
import Clckwrks.ProfileData.Acid
import Clckwrks.URL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe            (fromMaybe)
import Data.String           (fromString)
import Data.Text             (Text, pack)
import qualified Data.Text   as Text
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format      (formatTime)
import Data.UUID             (toString)
import Happstack.Server      (Response, ok, toResponseBS)
import HSP
import HSP.XML               (renderXML)
import System.Locale         (defaultTimeLocale)

atom :: FeedConfig  -- ^ feed configuration
     -> [Page]      -- ^ pages to publish in feed
     -> Clck ClckURL XML
atom FeedConfig{..} pages =
    unXMLGenT $ <feed xmlns="http://www.w3.org/2005/Atom">
                 <title><% feedTitle %></title>
                 <link type="text/html" href=Blog />
                 <link rel="self" type="application/atom+xml" href=AtomFeed />
                 <author>
                  <name><% feedAuthorName %></name>
                 </author>
                 <updated><% atomDate $ mostRecentUpdate pages %></updated>
                 <id><% "urn:uuid:" ++ toString feedUUID %></id>
                 <% mapM entry pages %>
                </feed>

mostRecentUpdate :: [Page]  -- ^ pages to consider
                 -> UTCTime -- ^ most recent updated time
mostRecentUpdate []    = posixSecondsToUTCTime 0
mostRecentUpdate pages =
    maximum $ map pageUpdated pages

entry :: Page
      -> Clck ClckURL XML
entry Page{..} =
     unXMLGenT $ <entry>
                   <title><% pageTitle %></title>
                   <link href=(ViewPage $ pageId) />
                   <id><% "urn:uuid:" ++ toString pageUUID %></id>
                   <% author %>
                   <updated><% atomDate pageUpdated %></updated>
                   <% atomContent pageSrc %>
                 </entry>
    where
      author :: XMLGenT (Clck ClckURL) XML
      author =
          do mu <- query $ UsernameForId pageAuthor
             case mu of
               Nothing -> return $ cdata ""
               (Just n)
                   | Text.null n ->
                       return $ cdata ""
                   | otherwise ->
                       <author>
                        <name><% n %></name>
                       </author>

atomDate :: UTCTime -> String
atomDate time =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time

atomContent :: Markup -> Clck ClckURL XML
atomContent markup =
    do c <- markupToContent markup
       case c of
         (PlainText txt) ->
              unXMLGenT $ <content type="text"><% txt %></content>
         (TrustedHtml html) ->
              unXMLGenT $ <content type="html"><% html %></content>

handleAtomFeed :: Clck ClckURL Response
handleAtomFeed =
    do ps <- query AllPosts
       feedConfig <- query GetFeedConfig
       xml <- atom feedConfig ps
       ok $ toResponseBS (fromString "application/atom+xml;charset=utf-8") (UTF8.fromString $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++ renderXML xml)

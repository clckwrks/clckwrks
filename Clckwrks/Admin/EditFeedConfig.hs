{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditFeedConfig where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.Page.Acid       (GetFeedConfig(..), SetFeedConfig(..))
import Data.Text                (Text, pack)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text

editFeedConfig :: ClckURL -> Clck ClckURL Response
editFeedConfig here =
    do feedConfig <- query $ GetFeedConfig
       action <- showURL here
       template "edit feed config" () $
                  <%>
                   <% reform (form action) "ep" updateFeedConfig Nothing (feedConfigForm feedConfig) %>
                  </%>
    where
      updateFeedConfig :: FeedConfig -> Clck ClckURL Response
      updateFeedConfig fc =
          do update (SetFeedConfig fc)
             seeOtherURL (Admin Console)

feedConfigForm :: FeedConfig -> ClckForm ClckURL FeedConfig
feedConfigForm fc@FeedConfig{..} =
    fieldset $
     ol $
      ((,) <$> (li $ label "Feed Title:")          ++> (li $ inputText feedTitle)
           <*>  (li $ label "Default Author Name:") ++> (li $ inputText feedTitle)
           <* inputSubmit (pack "update")
      )
     `transformEither` toFeedConfig
    where
      toFeedConfig :: (Text, Text) -> Either ClckFormError FeedConfig
      toFeedConfig (ttl, athr) =
              Right $ fc { feedTitle      = ttl
                         , feedAuthorName = athr
                         }

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.EditFeedConfig where

import Clckwrks
import Clckwrks.Admin.Template  (template)
import Clckwrks.FormPart        (FormDF, fieldset, ol, li, multiFormPart)
import Clckwrks.Page.Acid       (GetFeedConfig(..), SetFeedConfig(..))
import Data.Text                (Text)
import Text.Digestive           (Transformer, (++>), transform, transformEither)
import Text.Digestive.HSP.Html4 (inputText, label, submit)

editFeedConfig :: ClckURL -> Clck ClckURL Response
editFeedConfig here =
    do feedConfig <- query $ GetFeedConfig
       action <- showURL here
       template "edit feed config" () $
                  <%>
                   <% multiFormPart "ep" action updateFeedConfig Nothing (feedConfigFormlet feedConfig) %>
                  </%>
    where
      updateFeedConfig :: FeedConfig -> Clck ClckURL Response
      updateFeedConfig fc =
          do update (SetFeedConfig fc)
             seeOtherURL (Admin Console)

feedConfigFormlet :: FeedConfig -> FormDF (Clck ClckURL) FeedConfig
feedConfigFormlet fc@FeedConfig{..} =
    (fieldset $
       ol $ (,) <$> ((li $ label "Feed Title:")                ++>
                        (li $ inputText (Just feedTitle)))
                <*> ((li $ label "Default Author Name:")  ++>
                        (li $ inputText (Just feedAuthorName)))
                <*  submit "update")
    `transform` toFeedConfig
    where
      toFeedConfig :: (Monad m) => Transformer m e (Text, Text) FeedConfig
      toFeedConfig =
          transformEither $ \(ttl, athr) ->
              Right $ fc { feedTitle      = ttl
                         , feedAuthorName = athr
                         }

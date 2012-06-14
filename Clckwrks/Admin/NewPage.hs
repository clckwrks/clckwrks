{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.NewPage where

import Clckwrks
import Clckwrks.Page.Acid      as Acid
import Clckwrks.Admin.Template (template)
import Data.UUID               () -- instance Random UUID
import Data.Time.Clock         (getCurrentTime)
import System.Random           (randomIO)

newPage :: PageKind -> Clck AdminURL Response
newPage pageKind =
    do method GET
       template "Create New Page/Post" () $
         <%>
          <form action=Clckwrks.NewPage method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Page</button>
          </form>
          <form action=Clckwrks.NewPost method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Post</button>
          </form>
         </%>

    <|>
    do method POST
       uuid <- liftIO $ randomIO
       now  <- liftIO $ getCurrentTime
       muid <- getUserId
       case muid of
         Nothing -> escape $ internalServerError $ toResponse "Clcwrks.Admin.NewPage.newPage was unable to obtain the current UserId"
         (Just uid) ->
             do page <- update (Acid.NewPage pageKind uid uuid now)
                seeOtherURL (EditPage (pageId page))

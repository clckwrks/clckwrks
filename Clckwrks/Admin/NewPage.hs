{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.NewPage where

import Clckwrks
import Clckwrks.Page.Acid as Acid
import Clckwrks.Admin.Template (template)

newPage :: Clck AdminURL Response
newPage =
    do method GET
       template "Create New Page" () $
          <form action=Clckwrks.NewPage method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Page</button>
          </form>
    <|>
    do method POST
       page <- update Acid.NewPage
       seeOtherURL (EditPage (pageId page))

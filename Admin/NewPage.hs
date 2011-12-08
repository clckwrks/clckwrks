{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.NewPage where

import Clckwrks
import Page.Acid as Acid

newPage :: Clck AdminURL Response
newPage =
    do method POST
       page <- update Acid.NewPage
       seeOtherURL (EditPage (pageId page))

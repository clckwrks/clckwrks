{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Admin.PreviewPage
    ( previewPage
    ) where

import Clckwrks
import Clckwrks.Admin.Template   (template)
import Clckwrks.ProfileData.Acid (HasRole(..))
import Clckwrks.Page.Acid        (Page(..), PublishStatus(..), PageById(..))
import Clckwrks.Unauthorized     ()
import qualified Data.Set        as Set

previewPage :: Clck ClckURL Response -> PageId -> Clck ClckURL Response
previewPage pageHandler pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> do notFound ()
                       template "Page not found" () $ <% "Page not found: " ++ show (unPageId pid) %>
         (Just page) ->
           do muid <- getUserId
              authorized <-
                  case muid of
                    Nothing    -> return False
                    (Just uid) -> query $ HasRole uid (Set.singleton Administrator)
              if authorized
                 then do setCurrentPage pid
                         pageHandler
                 else unauthorized (toResponse $ "Sorry, you need Administrator access to view this page.")

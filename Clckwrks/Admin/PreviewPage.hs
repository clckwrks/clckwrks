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
import Control.Monad.State       (get)
import qualified Data.Set        as Set
import Web.Plugins.Core          (getTheme)

previewPage :: PageId -> Clck ClckURL Response
previewPage pid =
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
                         cs <- get
                         ttl <- getPageTitle
                         bdy <- getPageContent
                         mTheme <- getTheme (plugins cs)
                         case mTheme of
                           Nothing -> escape $ internalServerError $ toResponse "No theme package is loaded."
                           (Just theme) -> fmap toResponse $ unXMLGenT $ (_themeTemplate theme ttl () bdy)
                 else unauthorized (toResponse $ "Sorry, you need Administrator access to view this page.")

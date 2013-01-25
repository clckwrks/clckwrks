{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings #-}
module Clckwrks.Route where

import Clckwrks
import Clckwrks.Admin.Route        (routeAdmin)
import Clckwrks.BasicTemplate      (basicTemplate)
import Clckwrks.Monad              (calcTLSBaseURI, withAbs)
-- import Clckwrks.Page.Acid          (GetPageTitle(..), IsPublishedPage(..))
-- import Clckwrks.Page.Atom          (handleAtomFeed)
import Clckwrks.ProfileData.Route  (routeProfileData)
import Control.Monad.State         (MonadState(get))
import Data.Maybe                  (fromJust)
import Data.Monoid                 ((<>))
import qualified Data.Set          as Set
import Data.Text                   (Text)
import qualified Data.Text         as Text
import Happstack.Auth              (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Network.URI                 (unEscapeString)
import Paths_clckwrks              (getDataDir)
import System.FilePath             ((</>), makeRelative, splitDirectories)
import Web.Plugins.Core            (Plugin(..), addHandler, getConfig, getTheme, getPluginRouteFn, initPlugin)

checkAuth :: (Happstack m, Monad m) =>
             ClckURL
          -> ClckT ClckURL m ClckURL
checkAuth url =
    case url of
--      ViewPage{}           -> return url
--      ViewPageSlug{}       -> return url
--      Blog{}               -> return url
--      AtomFeed{}           -> return url
      ThemeData{}          -> return url
      PluginData{}         -> return url
      Admin{}              -> requiresRole (Set.singleton Administrator) url
      Auth{}               -> return url
      Profile EditProfileData{}    -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditProfileDataFor{} -> requiresRole (Set.fromList [Administrator]) url
      Profile CreateNewProfileData -> return url

routeClck :: ClckURL
          -> Clck ClckURL Response
routeClck url' =
    do url <- checkAuth url'
       setUnique 0
       case url of
{-
         (ViewPage pid) ->
           do r <- query (GetPageTitle pid)
              case r of
                Nothing ->
                    notFound $ toResponse ("Invalid PageId " ++ show (unPageId pid))
                (Just (title, slug)) ->
                    seeOtherURL (ViewPageSlug pid (toSlug title slug))

         (ViewPageSlug pid _slug) ->
           do published <- query (IsPublishedPage pid)
              if published
                 then do setCurrentPage pid
                         cs <- get
                         ttl <- getPageTitle
                         bdy <- getPageContent
                         themeTemplate (plugins cs) ttl () bdy
                 else do notFound $ toResponse ("Invalid PageId " ++ show (unPageId pid))

         (Blog) ->
           do p <- plugins <$> get
              mTheme <- getTheme p
              case mTheme of
                Nothing -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
                (Just theme) -> fmap toResponse $ unXMLGenT $ themeBlog theme

         AtomFeed ->
             do handleAtomFeed

-}
         (ThemeData fp')  ->
             do p      <- plugins <$> get
                mTheme <- getTheme p
                case mTheme of
                  Nothing -> notFound $ toResponse ("No theme package is loaded." :: Text)
                  (Just theme) ->
                      do fp    <- liftIO $ themeDataDir theme
                         let fp'' = makeRelative "/" (unEscapeString fp')
                         if not (isSafePath (splitDirectories fp''))
                           then notFound (toResponse ())
                           else serveFile (guessContentTypeM mimeTypes) (fp </> "data" </> fp'')

         (PluginData plugin fp')  ->
             do pp <- liftIO getDataDir
                let fp'' = makeRelative "/" (unEscapeString fp')
                if not (isSafePath (splitDirectories fp''))
                  then notFound (toResponse ())
                  else serveFile (guessContentTypeM mimeTypes) (pp </> "data" </> fp'')

         (Admin adminURL) ->
             routeAdmin adminURL

         (Profile profileDataURL) ->
             do nestURL Profile $ routeProfileData profileDataURL

         (Auth apURL) ->
             do clckState <- get
                cc <- getConfig (plugins clckState)
                let go = do let Acid{..} = acidState clckState
                            u <- showURL $ Profile CreateNewProfileData
                            withAbs $ nestURL Auth $ handleAuthProfile acidAuth acidProfile basicTemplate Nothing Nothing u apURL
                case clckTLS cc of
                  Nothing -> go
                  (Just tlsSettings) ->
                      do secure <- rqSecure <$> askRq
                         if secure
                            then go
                            else do u <- rqUri <$> askRq
                                    let sslU = ((Text.unpack $ fromJust $ calcTLSBaseURI cc) ++ u)
                                    seeOther sslU (toResponse ())



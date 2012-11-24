{-# LANGUAGE RecordWildCards, FlexibleContexts, Rank2Types, OverloadedStrings #-}
module Clckwrks.Plugin where

import Control.Applicative ((<$>))
import Control.Monad.State (MonadState(get))
import Clckwrks
import Clckwrks.Acid
import Clckwrks.Admin.Route        (routeAdmin)
import Clckwrks.Admin.Template     (defaultAdminMenu)
import Clckwrks.BasicTemplate      (basicTemplate)
import Clckwrks.Page.Acid          (GetPageTitle(..), IsPublishedPage(..))
import Clckwrks.Page.Atom          (handleAtomFeed)
import Clckwrks.ProfileData.Route  (routeProfileData)
import Clckwrks.Monad
import Clckwrks.URL
import Clckwrks.Server (checkAuth)
import Control.Monad
import Control.Monad.Trans
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Happstack.Server
import Happstack.Auth              (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Network.URI                 (unEscapeString)
import System.FilePath             ((</>), makeRelative, splitDirectories)
import Web.Routes                  hiding (nestURL)
import Web.Plugins.Core            (Plugin(..), addHandler, getTheme, getPluginRouteFn, initPlugin)

themeTemplate :: ( EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers
                 , EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body) =>
                 ClckPlugins
              -> Text
              -> headers
              -> body
              -> ClckT ClckURL (ServerPartT IO) Response
themeTemplate plugins ttl hdrs bdy =
    do mTheme <- getTheme plugins
       case mTheme of
         Nothing -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
         (Just theme) -> fmap toResponse $ unXMLGenT $ (_themeTemplate theme ttl hdrs bdy)


clckHandler :: (ClckURL -> [(Text, Maybe Text)] -> Text)
            -> ClckPlugins
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
clckHandler showRouteFn plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e) -> notFound $ toResponse (show e)
      (Right u) -> routeClck u

routeClck :: ClckURL -> Clck ClckURL Response
routeClck url' =
    do url <- checkAuth url'
       setUnique 0
       case url of
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
--                         ok $ toResponse (show pid)
--                         (clckPageHandler cc)
                 else do notFound $ toResponse ("Invalid PageId " ++ show (unPageId pid))
{-
         (Blog) ->  -- FIXME
           do clckBlogHandler cc
-}
         AtomFeed ->
             do handleAtomFeed

         (ThemeData fp')  ->
             do fp <- themePath <$> get
                let fp'' = makeRelative "/" (unEscapeString fp')
                if not (isSafePath (splitDirectories fp''))
                   then notFound (toResponse ())
                   else serveFile (guessContentTypeM mimeTypes) (fp </> "data" </> fp'')

         (PluginData plugin fp')  ->
             do ppm <- pluginPath <$> get
                case Map.lookup plugin ppm of
                  Nothing -> notFound (toResponse ())
                  (Just pp) ->
                      do let fp'' = makeRelative "/" (unEscapeString fp')
                         if not (isSafePath (splitDirectories fp''))
                           then notFound (toResponse ())
                           else serveFile (guessContentTypeM mimeTypes) (pp </> "data" </> fp'')

         (Admin adminURL) ->
             routeAdmin adminURL

         (Profile profileDataURL) ->
             do nestURL Profile $ routeProfileData profileDataURL

         (Auth apURL) ->
             do Acid{..} <- acidState <$> get
                u <- showURL $ Profile CreateNewProfileData
                nestURL Auth $ handleAuthProfile acidAuth acidProfile basicTemplate Nothing Nothing u apURL

clckInit :: ClckPlugins
         -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
--       evalClckT defaultAdminMenu clckShowFn
--       addPreProc plugins "clck" (clckPreProcessor clckShowFn)
       addHandler (plugins :: ClckPlugins) "clck" (clckHandler clckShowFn)
       return Nothing


clckPlugin :: Plugin ClckURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ([TL.Text -> ClckT ClckURL IO TL.Text])
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return ()
    }

plugin :: ClckPlugins
       -> Text
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI clckPlugin

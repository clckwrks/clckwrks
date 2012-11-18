{-# LANGUAGE RecordWildCards, FlexibleContexts, OverloadedStrings #-}
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
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Happstack.Server
import Happstack.Auth              (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Network.URI                 (unEscapeString)
import System.FilePath             ((</>), makeRelative, splitDirectories)
import Web.Routes                  hiding (nestURL)
import Web.Plugin.Core
{-
themeTemplate :: ( EmbedAsChild (ServerPartT IO) headers
                 , EmbedAsChild (ServerPartT IO) body
                 ) =>
                 Plugins Theme (ClckT ClckURL (ServerPartT IO) XML)
              -> Text
              -> headers
              -> body
              -> ClckT ClckURL (ServerPartT IO) Response
themeTemplate plugins title headers body =
    do mTheme <- getTheme plugins
       case mTheme of
         Nothing -> escape $ internalServerError $ toResponse $ ("No theme package is loaded." :: Text)
         (Just theme) -> fmap toResponse $ unXMLGenT $ (_themeTemplate theme) title headers body
-}

themeTemplate
  :: (EmbedAsChild (ClckT ClckURL (ServerPartT IO)) headers,
      EmbedAsChild (ClckT ClckURL (ServerPartT IO)) body) =>
     Plugins Theme n hook
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
            -> Plugins Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ())
            -> [Text]
            -> ClckT ClckURL (ServerPartT IO) Response
clckHandler showRouteFn plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e) -> notFound $ toResponse (show e)
      (Right u) -> routeClck u
{-
      (Right (ViewPage _)) ->
          do pps <- liftIO $ getPreProcs plugins
             txt <- liftIO $ foldM (\txt pp -> pp txt) "I like cheese." (Map.elems pps)
--             themeTemplate plugins "cheese." () <p><% txt %></p>
             ok $ toResponse txt
-}

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
         (Blog) ->
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

clckInit :: Plugins Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
--       evalClckT defaultAdminMenu clckShowFn
--       addPreProc plugins "clck" (clckPreProcessor clckShowFn)
       addHandler plugins "clck" (clckHandler clckShowFn)
       return Nothing


clckPlugin :: Plugin ClckURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ())
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathInfo
    , pluginPostHook   = return ()
    }

plugin :: Plugins Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) -> Text -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI clckPlugin

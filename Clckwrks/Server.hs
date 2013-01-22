{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards #-}
module Clckwrks.Server where

import Clckwrks
import Clckwrks.BasicTemplate       (basicTemplate)
import Clckwrks.Admin.Route         (routeAdmin)
import Clckwrks.Admin.Template      (defaultAdminMenu)
import Clckwrks.Monad               (ClckwrksConfig(..), TLSSettings(..))
import Clckwrks.Page.Acid           (GetPageTitle(..), IsPublishedPage(..))
import Clckwrks.Page.Atom           (handleAtomFeed)
import Clckwrks.Page.PreProcess     (pageCmd)
import Clckwrks.ProfileData.Route   (routeProfileData)
import Clckwrks.ProfileData.Types   (Role(..))
import Clckwrks.ProfileData.URL     (ProfileDataURL(..))
import Control.Arrow                (second)
import Control.Concurrent           (forkIO, killThread)
import Control.Concurrent.STM       (atomically, newTVar)
import Control.Monad.State          (get, evalStateT)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import Data.Maybe                   (fromJust)
import Data.Monoid                  ((<>))
import Data.String                  (fromString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.UUID          as UUID
import Happstack.Auth               (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Happstack.Server.SimpleHTTPS (TLSConf(..), nullTLSConf, simpleHTTPS)
import Network.URI                  (unEscapeString)
import System.FilePath              ((</>), makeRelative, splitDirectories)
import Web.Routes.Happstack         (implSite)
import Web.Plugins.Core             (Plugins, withPlugins, getPluginRouteFn, getPostHooks, serve)
import qualified Paths_clckwrks     as Clckwrks

withClckwrks :: ClckwrksConfig -> (ClckState -> IO b) -> IO b
withClckwrks cc action =
    withPlugins cc [] $ \plugins ->
       withAcid (fmap (\top -> top </> "_state") (clckTopDir cc)) $ \acid ->
           do u <- atomically $ newTVar 0
              let clckState = ClckState { acidState        = acid
                                        , currentPage      = PageId 0
                                        , uniqueId         = u
                                        , adminMenus       = []
                                        , enableAnalytics  = clckEnableAnalytics cc
                                        , plugins          = plugins
                                        }
              action clckState

simpleClckwrks :: ClckwrksConfig -> IO ()
simpleClckwrks cc =
  withClckwrks cc $ \clckState ->
      do (clckState', cc') <- (clckInitHook cc) (calcBaseURI cc) clckState cc
         let p = plugins clckState'
         hooks <- getPostHooks p
         (Just clckShowFn) <- getPluginRouteFn p "clck"
         let showFn = \url params -> clckShowFn url []
         clckState'' <- execClckT showFn clckState' $ do sequence_ hooks
                                                         dm <- defaultAdminMenu
                                                         mapM_ addAdminMenu dm
         httpTID  <- forkIO $ simpleHTTP (nullConf { port = clckPort cc' }) (handlers cc' clckState'')

         mHttpsTID <-
             case clckTLS cc' of
               Nothing -> return Nothing
               (Just TLSSettings{..}) ->
                   do let tlsConf = nullTLSConf { tlsPort = clckTLSPort
                                                , tlsCert = clckTLSCert
                                                , tlsKey  = clckTLSKey
                                                }
                      tid <- forkIO $ simpleHTTPS tlsConf (handlers cc' clckState'')
                      return (Just tid)
         -- putStrLn "Server Now Listening For Requests."
         waitForTermination
         killThread httpTID
         maybe (return ()) killThread mHttpsTID

    where
    handlers cc clckState =
       do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
          msum $
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ (liftIO $ Clckwrks.getDataFileName "static") >>= serveDirectory DisableBrowsing []
            , nullDir >> seeOther ("/clck/view-page/1" :: String) (toResponse ())
            , clckSite cc clckState
            ]

jsHandlers :: (Happstack m) => ClckwrksConfig -> m Response
jsHandlers c =
  msum [ dir "jquery"      $ serveDirectory DisableBrowsing [] (clckJQueryPath c)
       , dir "jquery-ui"   $ serveDirectory DisableBrowsing [] (clckJQueryUIPath c)
       , dir "jstree"      $ serveDirectory DisableBrowsing [] (clckJSTreePath c)
       , dir "json2"       $ serveDirectory DisableBrowsing [] (clckJSON2Path c)
       ]

clckSite :: ClckwrksConfig -> ClckState -> ServerPart Response
clckSite cc clckState =
    do (Just clckShowFn) <- getPluginRouteFn (plugins clckState) (Text.pack "clck")
       evalClckT clckShowFn clckState (pluginsHandler (plugins clckState))

pluginsHandler :: (Functor m, ServerMonad m, FilterMonad Response m, MonadIO m) =>
               Plugins theme (m Response) hook config ppm
            -> m Response
pluginsHandler plugins =
    do paths <- (map Text.pack . rqPaths) <$> askRq
       case paths of
         (p : ps) ->
             do e <- liftIO $ serve plugins p ps
                case e of
                  (Right c) -> c
                  (Left e) -> notFound $ toResponse e
         _ -> notFound (toResponse ())

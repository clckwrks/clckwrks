{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards #-}
module Clckwrks.Server where

import Clckwrks
import Clckwrks.Admin.Route         (routeAdmin)
import Clckwrks.Monad               (ClckwrksConfig(..), TLSSettings(..), calcBaseURI, calcTLSBaseURI, initialClckPluginsSt)
-- import Clckwrks.Page.Acid           (GetPageTitle(..), IsPublishedPage(..))
-- import Clckwrks.Page.Atom           (handleAtomFeed)
-- import Clckwrks.Page.PreProcess     (pageCmd)
import Clckwrks.ProfileData.Route   (routeProfileData)
import Clckwrks.ProfileData.Types   (Role(..))
import Clckwrks.ProfileData.URL     (ProfileDataURL(..))
import Control.Arrow                (second)
import Control.Concurrent           (forkIO, killThread)
import Control.Concurrent.STM       (atomically, newTVar)
import Control.Monad.State          (get, evalStateT)
import qualified Data.ByteString.Char8 as B
import Data.Acid.Advanced           (query')
import           Data.Map           (Map)
import qualified Data.Map           as Map
import Data.Maybe                   (fromJust, fromMaybe, isNothing)
import Data.Monoid                  ((<>))
import Data.String                  (fromString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.UUID          as UUID
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Happstack.Server.SimpleHTTPS (TLSConf(..), nullTLSConf, simpleHTTPS)
import System.FilePath              ((</>), makeRelative, splitDirectories)
import Web.Routes.Happstack         (implSite)
import Web.Plugins.Core             (Plugins, withPlugins, getPluginRouteFn, getPostHooks, serve)
import qualified Paths_clckwrks     as Clckwrks

withClckwrks :: ClckwrksConfig -> (ClckState -> IO b) -> IO b
withClckwrks cc action = do
  let top' = fmap (\top -> top </> "_state") (clckTopDir cc)
  withAcid top' $ \acid ->
    withPlugins cc (initialClckPluginsSt acid) $ \plugins -> do
      u <- atomically $ newTVar 0
      let clckState = ClckState { acidState        = acid
--                                        , currentPage      = PageId 0
                                , uniqueId         = u
                                , adminMenus       = []
                                , enableAnalytics  = clckEnableAnalytics cc
                                , plugins          = plugins
                                , requestInit      = return ()
                                }
      action clckState

simpleClckwrks :: ClckwrksConfig -> IO ()
simpleClckwrks cc =
  withClckwrks cc $ \clckState ->
      do let baseURI =
               case calcTLSBaseURI cc of
                 (Just baseUri) -> baseUri
                 Nothing -> calcBaseURI cc
         (clckState', cc') <- (clckInitHook cc) baseURI clckState cc
         let p = plugins clckState'
         hooks <- getPostHooks p
         (Just clckShowFn) <- getPluginRouteFn p "clck"
         let showFn = \url params -> clckShowFn url []
         clckState'' <- execClckT showFn clckState' $ sequence_ hooks

         mHttpsTID <-
             case clckTLS cc' of
               Nothing -> return Nothing
               (Just TLSSettings{..}) ->
                   do let tlsConf = nullTLSConf { tlsPort = clckTLSPort
                                                , tlsCert = clckTLSCert
                                                , tlsKey  = clckTLSKey
                                                , tlsCA   = clckTLSCA
                                                }
                      tid <- forkIO $ simpleHTTPS tlsConf (handlers cc' clckState'')
                      return (Just tid)

         httpTID  <- if isNothing mHttpsTID
                       then forkIO $ simpleHTTP (nullConf { port = clckPort cc' }) (handlers cc' clckState'')
                       else forkIO $ simpleHTTP (nullConf { port = clckPort cc' }) forceHTTPS
         -- putStrLn "Server Now Listening For Requests."
         waitForTermination
         killThread httpTID
         maybe (return ()) killThread mHttpsTID

    where
      handlers :: ClckwrksConfig -> ClckState -> ServerPart Response
      handlers cc clckState =
       do forceCanonicalHost
          decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
          requestInit clckState
          msum $
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ (liftIO $ Clckwrks.getDataFileName "static") >>= serveDirectory DisableBrowsing []
            , do nullDir
                 mRR <- query' (acidCore . acidState $ clckState) GetRootRedirect
                 seeOther (fromMaybe ("/page/view-page/1") mRR) (toResponse ()) -- FIXME: get redirect location from database
            , clckSite cc clckState
            ]

      forceCanonicalHost :: ServerPart ()
      forceCanonicalHost =
          do rq <- askRq
             case getHeader "host" rq of
               Nothing -> return ()
               (Just hostBS) ->
                   if (clckHostname cc == (B.unpack $ B.takeWhile (/= ':') hostBS))
                   then return ()
                   else escape $ seeOther ((if rqSecure rq then (fromJust $ calcTLSBaseURI cc) else (calcBaseURI cc)) <> (Text.pack $ rqUri rq) <> (Text.pack $ rqQuery rq)) (toResponse ())

      -- if https:// is available, then force it to be used.
      -- GET requests will be redirected automatically, POST, PUT, etc will be denied
      forceHTTPS :: ServerPart Response
      forceHTTPS =
          msum [ do method GET
                    rq <- askRq
                    seeOther ((fromJust $ calcTLSBaseURI cc) <> (Text.pack $ rqUri rq) <> (Text.pack $ rqQuery rq)) (toResponse ())
               , do forbidden (toResponse ("https:// required." :: Text))
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

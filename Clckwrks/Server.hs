{-# LANGUAGE RecordWildCards #-}
module Clckwrks.Server where

import Clckwrks
import Clckwrks.Admin.Route       (routeAdmin)
import Clckwrks.Admin.Template    (template)
import Clckwrks.ProfileData.Route (routeProfileData)
import Clckwrks.ProfileData.URL   (ProfileDataURL(..))
import Control.Concurrent.STM     (atomically, newTVar)
import Control.Monad.State        (get, evalStateT)
import qualified Data.Map         as Map
import qualified Data.Text        as Text
import Data.String                (fromString)
import Happstack.Auth             (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import System.FilePath            ((</>), makeRelative, splitDirectories)
import Web.Routes.Happstack       (implSite)

data ClckwrksConfig url = ClckwrksConfig
    { clckHostname     :: String
    , clckPort         :: Int
    , clckURL          :: ClckURL -> url
    , clckJQueryPath   :: FilePath
    , clckJQueryUIPath :: FilePath
    , clckJSTreePath   :: FilePath
    , clckJSON2Path    :: FilePath
    , clckThemeDir     :: FilePath
    , clckStaticDir    :: FilePath
    , clckPageHandler  :: Clck ClckURL Response
    }
    
        
withClckwrks :: ClckwrksConfig url -> (ClckState -> IO b) -> IO b
withClckwrks cc action =
    do withAcid Nothing $ \acid ->
           do u <- atomically $ newTVar 0
              let clckState = ClckState { acidState        = acid 
                                        , currentPage      = PageId 0
                                        , themePath        = clckThemeDir cc
                                        , componentPrefix  = Prefix (fromString "clckwrks")
                                        , uniqueId         = u
                                        , preProcessorCmds = Map.empty
                                        , adminMenus       = []
                                        }
              action clckState
  
simpleClckwrks :: ClckwrksConfig u -> IO ()
simpleClckwrks cc =
  withClckwrks cc $ \clckState ->
    simpleHTTP (nullConf { port = clckPort cc }) (handlers (clckPageHandler cc) clckState)
  where
    handlers ph clckState =
       do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
          msum $ 
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite ph clckState)
            ]
              
jsHandlers :: (Happstack m) => ClckwrksConfig u -> m Response
jsHandlers c =
  msum [ dir "jquery"      $ serveDirectory DisableBrowsing [] (clckJQueryPath c)
       , dir "jquery-ui"   $ serveDirectory DisableBrowsing [] (clckJQueryUIPath c)
       , dir "jstree"      $ serveDirectory DisableBrowsing [] (clckJSTreePath c)
       , dir "json2"       $ serveDirectory DisableBrowsing [] (clckJSON2Path c)
       ]

routeClck :: Clck ClckURL Response -> ClckURL -> Clck ClckURL Response
routeClck pageHandler url =
    do setUnique 0
       case url of
         (ViewPage pid) ->
           do setCurrentPage pid
              pageHandler
         (ThemeData fp')  ->
             do fp <- themePath <$> get
                let fp'' = makeRelative "/" fp'
                if not (isSafePath (splitDirectories fp''))
                   then notFound (toResponse ())
                   else serveFile (guessContentTypeM mimeTypes) (fp </> "data" </> fp'')
         (Admin adminURL) ->
             routeAdmin adminURL
         (Profile profileDataURL) ->
             nestURL Profile $ routeProfileData profileDataURL
         (Auth apURL) ->
             do Acid{..} <- acidState <$> get
                u <- showURL $ Profile CreateNewProfileData
                nestURL Auth $ handleAuthProfile acidAuth acidProfile template Nothing Nothing u apURL

routeClck' :: Clck ClckURL Response -> ClckState -> ClckURL -> RouteT ClckURL (ServerPartT IO) Response
routeClck' pageHandler clckState url =
    mapRouteT (\m -> evalStateT m clckState) $ (unClckT $ routeClck pageHandler url) 

clckSite :: Clck ClckURL Response -> ClckState -> Site ClckURL (ServerPart Response)
clckSite ph clckState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u = unRouteT (routeClck' ph clckState u) f

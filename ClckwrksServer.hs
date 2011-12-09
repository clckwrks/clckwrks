{-# LANGUAGE RecordWildCards #-}
module ClckwrksServer where

import Admin.Route          (routeAdmin)
import Admin.Template       (template)
import Control.Monad.State  (get, evalStateT)

import Clckwrks
import qualified Data.Text as Text
import Data.String          (fromString)
import Happstack.Auth
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import System.FilePath     ((</>), makeRelative, splitDirectories)
-- import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins)
-- import System.Plugins.Auto.Reloader (func)
import ProfileData.Route    (routeProfileData)
import ProfileData.URL      (ProfileDataURL(..))
import Web.Routes.Happstack (implSite)

data ClckwrksConfig url = ClckwrksConfig
    { clckHostname     :: String
    , clckPort         :: Int
    , clckURL          :: ClckURL -> url
    , clckJQueryPath   :: FilePath
    , clckJQueryUIPath :: FilePath
    , clckJSTreePath   :: FilePath
    , clckJSON2Path    :: FilePath
    , clckPageHandler  :: Clck ClckURL Response
    }
    
        
withClckwrks ::  (ClckState -> IO b) -> IO b
withClckwrks action =
    do -- ph <- initPlugins  let c = defaultClckwrksConfig  { clckURL = C }
       withAcid Nothing $ \acid ->
           do let clckState = ClckState { acidState       = acid 
                                        , currentPage     = PageId 0
                                        , themePath       = "../clckwrks-theme-basic/"
                                        , componentPrefix = Prefix (fromString "clckwrks")
                                        , uniqueId        = 0
                                        }
              action clckState
  
simpleClckwrks :: ClckwrksConfig u -> IO ()
simpleClckwrks cc =
  withClckwrks $ \clckState ->
    simpleHTTP (nullConf { port = clckPort cc }) (handlers (clckPageHandler cc) clckState)
  where
    handlers ph clckState =
       msum $ 
         [ jsHandlers cc
         , dir "favicon.ico" $ notFound (toResponse ())
         , dir "static"      $ serveDirectory DisableBrowsing [] "../static"
         , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite ph clckState)
         ]
              
jsHandlers :: ClckwrksConfig u -> ServerPart Response
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

clckSite :: Clck ClckURL Response -> ClckState -> Site ClckURL (ServerPart Response)
clckSite ph cmsState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m cmsState) $ unRouteT (unClck $ routeClck ph u) f

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
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins)
import System.Plugins.Auto.Reloader (func)
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
    }
    
defaultClckwrksConfig :: ClckwrksConfig ClckURL
defaultClckwrksConfig = ClckwrksConfig
      { clckHostname     = "localhost"
      , clckPort         = 8000 
      , clckURL          = id
      , clckJQueryPath   = "/usr/share/javascript/jquery/"
      , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
      , clckJSTreePath   = "../jstree/"
      , clckJSON2Path    = "../json2/"
      }
        
withClckwrks ::  (PluginHandle -> ClckState -> IO b) -> IO b
withClckwrks action =
    do ph <- initPlugins
       withAcid Nothing $ \acid ->
           do let clckState = ClckState { acidState       = acid 
                                        , currentPage     = PageId 0
                                        , themePath       = "../clckwrks-theme-basic/"
                                        , componentPrefix = Prefix (fromString "clckwrks")
                                        , uniqueId        = 0
                                        }
              action ph clckState
  
simpleClckwrks :: ClckwrksConfig u -> IO ()
simpleClckwrks cc =
  withClckwrks $ \ph clckState ->
    simpleHTTP (nullConf { port = clckPort cc }) (handlers ph clckState)
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

routeClck :: PluginHandle -> ClckURL -> Clck ClckURL Response
routeClck ph url =
    do setUnique 0
       case url of
         (ViewPage pid) ->
             do setCurrentPage pid
                fp <- themePath <$> get
                let pc = (defaultPluginConf { pcGHCArgs = [ "-i" ++ fp] 
--                                            , pcWhenWatched = \fp -> print fp
--                                            , pcWhenChanged = \fp -> print fp
                                            })
                withSymbol ph "PageMapper.hs" "pageMapper" pc page
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

clckSite :: PluginHandle -> ClckState -> Site ClckURL (ServerPart Response)
clckSite ph cmsState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m cmsState) $ unRouteT (unClck $ routeClck ph u) f

withSymbol :: (MonadIO m) => PluginHandle -> FilePath -> String -> PluginConf -> (a -> m b) -> m b
withSymbol ph fp sym pc f =
    do (errs, r) <- liftIO $ func ph fp sym pc
       case r of
         Nothing  -> error (unlines errs)
         (Just a) -> f a

page :: XMLGenT (Clck url) XML -> Clck url Response
page (XMLGenT part) = toResponse <$> part

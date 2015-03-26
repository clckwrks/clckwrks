{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings #-}
module Clckwrks.Route where

import Clckwrks
import Clckwrks.Admin.Route        (routeAdmin)
import Clckwrks.BasicTemplate      (basicTemplate)
import Clckwrks.Monad              (calcTLSBaseURI, withAbs)
import Clckwrks.ProfileData.API    (requiresRole)
import Clckwrks.ProfileData.Route  (routeProfileData)
import Clckwrks.JS.Route           (routeJS)
import Control.Monad.State         (MonadState(get))
import Data.Maybe                  (fromJust)
import Data.Monoid                 ((<>))
import qualified Data.Set          as Set
import Data.Text                   (Text, pack)
import qualified Data.Text         as Text
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
      ThemeData{}          -> return url
      ThemeDataNoEscape{}  -> return url
      PluginData{}         -> return url
      Admin{}              -> requiresRole (Set.singleton Administrator) url
      JS   {}              -> return url
      Profile EditProfileData{}    -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditNewProfileData{} -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditProfileDataFor{} -> requiresRole (Set.fromList [Administrator]) url
      Profile CreateNewProfileData -> return url

routeClck :: ClckURL
          -> Clck ClckURL Response
routeClck url' =
    do url <- checkAuth url'
       setUnique 0
       case url of
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
                           else serveFile (guessContentTypeM mimeTypes) (fp </> fp'')

         (ThemeDataNoEscape (NoEscape fp'))  ->
             do p      <- plugins <$> get
                mTheme <- getTheme p
                case mTheme of
                  Nothing -> notFound $ toResponse ("No theme package is loaded." :: Text)
                  (Just theme) ->
                      do fp    <- liftIO $ themeDataDir theme
                         let fp'' = makeRelative "/" fp'
                         if not (isSafePath (splitDirectories fp''))
                           then notFound (toResponse ())
                           else serveFile (guessContentTypeM mimeTypes) (fp </> fp'')

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

         (JS jsURL) ->
             do nestURL JS $ routeJS jsURL

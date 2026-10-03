{-# LANGUAGE DataKinds, FlexibleContexts, RecordWildCards, MultiParamTypeClasses, OverloadedStrings #-}
module Clckwrks.Route where

import AccessControl.Check         (Access(..))
import AccessControl.Schema
import AccessControl.Relation
import Clckwrks
import Clckwrks.AccessControl      (checkAccess)
import Clckwrks.Acid               (GetEnableOpenId(..))
import Clckwrks.Admin.Route        (routeAdmin)
import Clckwrks.BasicTemplate      (basicTemplate)
import Clckwrks.Monad              (calcTLSBaseURI, withAbs, query)
import Clckwrks.ProfileData.API    (requiresRole)
import Clckwrks.ProfileData.Route  (routeProfileData)
import Clckwrks.Rebac.Route        (routeRebac)
import qualified Clckwrks.Rebac.Route as Rebac
import Control.Monad.State         (MonadState(get))
import Data.Maybe                  (fromJust)
import Data.Monoid                 ((<>))
import qualified Data.Set          as Set
import Data.Text                   (Text, pack)
import qualified Data.Text         as Text
import qualified Data.Text.Lazy    as TL
import Data.Time.Clock.POSIX       (getPOSIXTime)
import Data.UserId                 (UserId(..))
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
      Admin{}              ->
        do roles <- getUserRoles            -- first check if the user has the Admin role using the old system
           if Set.member Administrator roles
             then pure url
             else do let clckAdmin = Object (ObjectType "clck") (ObjectId "admin") :: Object NoWildcard
                     now <- liftIO getPOSIXTime
                     r <- checkAccess clckAdmin (Permission "admin") (Just now)
                     case r of
                       Allowed -> pure url
                       (NotAllowed reasons) ->
                         do rq <- askRq
                            escape $ do setRedirectCookie (rqUri rq ++ rqQuery rq)
                                        -- FIXME; redirect after login
                                        unauthorizedPage  ("You do not have permission to view this page.") -- <> (TL.pack (show reasons)))


      Profile EditProfileData{}    -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditNewProfileData{} -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditProfileDataFor{} -> requiresRole (Set.fromList [Administrator]) url
      Profile CreateNewProfileData -> return url
      Rebac rebacURL ->
        do u' <- Rebac.checkAuth rebacURL
           pure $ Rebac u'

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

         (Rebac rebacURL) ->
             do nestURL Rebac $ routeRebac rebacURL

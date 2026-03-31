{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Rebac.Route where

import Clckwrks.AccessControl      (assertAccess, checkAccess)
import Control.Applicative         ((<$>))
import Control.Monad.Trans         (liftIO)
import Clckwrks.Monad              (ClckT(..), plugins)
import Clckwrks.Rebac.Page.Relations (relationsPanel)
import Clckwrks.Rebac.Page.RelationLog (relationLogPanel)
import Clckwrks.Rebac.Page.Schema    (schemaPanel)
import Clckwrks.Rebac.Page.ViewSchema (viewSchema)
import Clckwrks.Rebac.Types        (RebacPermission(..))
import Clckwrks.Rebac.URL          (RebacURL(..))
import Clckwrks.URL                (ClckURL)
import Control.Monad.State         (get)
import Control.Monad.Trans         (lift)
import qualified Data.Set          as Set
import Data.Time.Clock.POSIX       (getPOSIXTime)
import Happstack.Server            (Happstack, Response, ServerPartT, ok, toResponse)
import Web.Routes                  (RouteT(..), askRouteFn, runRouteT)
import Web.Plugins.Core            (getPluginRouteFn, pluginName)

-- | routeAuth
-- there is much craziness here. This should be more like clckwrks-plugin-page or something
routeRebac :: RebacURL
          -> ClckT RebacURL (ServerPartT IO) Response
routeRebac u' =
  do now <- liftIO getPOSIXTime
     u <- checkAuth u'
     case u of
       SchemaPanel      -> schemaPanel u
       (ViewSchema sid) -> viewSchema u sid
       RelationsPanel   -> relationsPanel u
       RelationLogPanel -> relationLogPanel u

{-
     p <- plugins <$> get
     ~(Just clckShowFn) <- getPluginRouteFn p "clck" -- (pluginName clckPlugin) -- a mildly dangerous hack to avoid circular depends
     let withClckURL m =  ClckT $ RouteT $ \_ -> unRouteT (unClckT m) clckShowFn
     case u of
       (Auth authenticateURL) ->
         do p <- plugins <$> get
            ~(Just authShowFn) <- getPluginRouteFn p "rebac"
            lift $ runRouteT routeAuthenticate (authShowFn . Auth) authenticateURL
-}

checkAuth :: (Happstack m, Monad m) =>
             RebacURL
          -> ClckT url m RebacURL
checkAuth url =
  do -- p <- plugins <$> get
     -- ~(Just clckShowFn) <- getPluginRouteFn p "clck" -- (pluginName clckPlugin) -- a mildly dangerous hack to avoid circular depends
     -- let requiresRole = requiresRole_ clckShowFn
     now <- liftIO getPOSIXTime
     case url of
       SchemaPanel         ->
         do assertAccess url RebacView (Just now)
            pure url
       (ViewSchema sid)    ->
         do assertAccess url RebacView (Just now)
            pure url
       RelationsPanel      ->
         do assertAccess url RebacView (Just now)
            pure url
       RelationLogPanel    ->
         do assertAccess url RebacView (Just now)
            pure url

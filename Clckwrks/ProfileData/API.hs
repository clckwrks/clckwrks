{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Clckwrks.ProfileData.API
    ( getProfileData
    , getUsername
    , getUserRoles
    , requiresRole
    , requiresRole_
    , whoami
    )  where

import Clckwrks.Acid  (Acid(..))
import Clckwrks.Monad
import Clckwrks.URL                 (ClckURL)
import {-# SOURCE #-} Clckwrks.Authenticate.Plugin (getUserId)
import Clckwrks.ProfileData.Acid
import Clckwrks.ProfileData.Types
import Clckwrks.Unauthorized        (unauthorizedPage)
import Control.Applicative          ((<$>))
import Control.Monad.State          (get)
import Control.Monad.Trans          (MonadIO)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import Data.Text                    (Text)
import qualified Data.Text.Lazy     as TL
import Data.UserId                  (UserId(..))
import Happstack.Server             (Happstack, askRq, escape, rqUri, rqQuery)
import Web.Routes                   (RouteT(..))

getProfileData :: UserId -> Clck url ProfileData
getProfileData uid = query (GetProfileData uid)

getUsername :: UserId -> Clck url Text
getUsername uid = query (GetUsername uid)

whoami :: Clck url (Maybe UserId)
whoami = getUserId

requiresRole_ :: (Happstack  m) => (ClckURL -> [(Text, Maybe Text)] -> Text) -> Set Role -> url -> ClckT u m url
requiresRole_ showFn role url =
    ClckT $ RouteT $ \_ -> unRouteT (unClckT (requiresRole role url)) showFn

requiresRole :: (Happstack m) => Set Role -> url -> ClckT ClckURL m url
requiresRole role url =
    do mu <- getUserId
       case mu of
         Nothing ->
             do rq <- askRq
                escape $ do setRedirectCookie (rqUri rq ++ rqQuery rq)
                            -- FIXME; redirect after login
                            unauthorizedPage  ("You do not have permission to view this page." :: TL.Text)
--                            seeOtherURL (Auth $ AuthURL A_Login)
         (Just uid) ->
             do r <- query (HasRole uid role)
                if r
                   then return url
                   else escape $ unauthorizedPage ("You do not have permission to view this page." :: TL.Text)

getUserRoles :: (Happstack m, MonadIO m) => ClckT u m (Set Role)
getUserRoles =
    do mu <- getUserId
       case mu of
         Nothing -> return (Set.singleton Visitor)
         (Just u) -> query (GetRoles u)

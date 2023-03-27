{-# LANGUAGE FlexibleContexts, TypeFamilies, QuasiQuotes, OverloadedStrings #-}
module Clckwrks.Authenticate.Page.ViewUsers where

import Clckwrks.Admin.Template (template)
import Clckwrks.Monad
import Clckwrks.URL             (ClckURL(..))
import Clckwrks.Authenticate.Monad ()
import Clckwrks.ProfileData.URL(ProfileDataURL(..))
import Data.UserId              (UserId(..))
import Data.Maybe               (maybe)
import Data.Foldable                 (toList)
import qualified Data.Text      as Text
import Happstack.Server         (Response, ServerPartT, ok, toResponse)
import Happstack.Authenticate.Core (Email(..), User(..), Username(..))
import Happstack.Authenticate.Handlers (GetUsers(..))
import Language.Haskell.HSX.QQ (hsx)
import Web.Plugins.Core            (Plugin(..), getPluginState)
import Web.Routes (showURL)

viewUsers :: ClckT ClckURL (ServerPartT IO) Response
viewUsers =
  do us <- query GetUsers
     template "View Users" () $ [hsx|
        <div>
          <h2>Users</h2>
          <table class="table">
           <thead>
            <tr><th>UserId</th><th>Username</th><th>Email</th></tr>
           </thead>
           <tbody>
             <% mapM mkRow (toList us) %>
           </tbody>
          </table>
        </div> |]
         where
           mkRow u =
             do epdf <- showURL (Profile (EditProfileDataFor (_userId u)))
                [hsx| <tr><td><a href=(Text.unpack epdf)><% show $ _unUserId $ _userId u %></a></td><td><% _unUsername $ _username u %></td><td><% maybe (Text.empty) _unEmail (_email u) %></td></tr> |]


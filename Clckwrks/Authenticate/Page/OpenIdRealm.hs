{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
module Clckwrks.Authenticate.Page.OpenIdRealm where

import Clckwrks.Admin.Template (template)
import Clckwrks.Monad
import Clckwrks.URL             (ClckURL)
import Happstack.Server         (Response, ServerPartT, ok, toResponse)
import Language.Haskell.HSX.QQ (hsx)

openIdRealmPanel :: ClckT ClckURL (ServerPartT IO) Response
openIdRealmPanel =
    do template "Set OpenId Realm" () $ [hsx|
        <div ng-controller="OpenIdCtrl">
         <openid-realm />
        </div> |]


{-# LANGUAGE TypeFamilies, QuasiQuotes, OverloadedStrings #-}
module Clckwrks.Authenticate.Page.ChangePassword where

import Clckwrks.Admin.Template (template)
import Clckwrks.Monad
import Clckwrks.URL             (ClckURL)
import Happstack.Server         (Response, ServerPartT, ok, toResponse)
import Language.Haskell.HSX.QQ (hsx)

changePasswordPanel :: ClckT ClckURL (ServerPartT IO) Response
changePasswordPanel =
    do template "Upload Medium" () $ [hsx|
        <%>
         <up-change-password />
        </%> |]


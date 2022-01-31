{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Clckwrks.Admin.EditSettings where

import Clckwrks                  hiding (transform)
import Clckwrks.Acid             (GetUACCT(..), SetUACCT(..), coreSiteName, coreUACCT, coreRootRedirect, coreLoginRedirect)
import Clckwrks.Admin.Template   (template)
import Control.Lens              ((&), (.~))
import Data.Maybe                (fromMaybe)
import Data.Text            (Text, pack, unpack)
import qualified Data.Text  as T

-- import Clckwrks.Page.Acid       (GetUACCT(..), SetUACCT(..))
import HSP.Google.Analytics      (UACCT(..))
import HSP.XMLGenerator
import HSP.XML                   (fromStringLit)
import Numeric                   (readDec)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.HSP.Text

viewUsers :: ClckURL -> Clck ClckURL Response
viewUsers here =
    do coreState <- query $ GetCoreState
       action <- showURL here
       template "Edit Settings" () $
                  <%>
                   <% reform (form action) "ss" updateSettings Nothing (editSettingsForm coreState) %>
                  </%>
    where
      updateSettings :: CoreState -> Clck ClckURL Response
      updateSettings coreState =
          do update (SetCoreState coreState)
             seeOtherURL here

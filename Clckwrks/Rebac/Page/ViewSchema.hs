{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings, QuasiQuotes, TypeFamilies #-}
module Clckwrks.Rebac.Page.ViewSchema where

import AccessControl.Schema          (KnownPermission, Permission(..), Schema(..), ToPermission(..), parseSchema, ppSchema)
import AccessControl.Relation        (ToObject(..), Object(..), ObjectId(..), ObjectType(..), Relation(..), ToRelation(..), ppRelationTuples)
import Clckwrks
import Clckwrks.Monad              (plugins)
import Clckwrks.Admin.Template     (template)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..))
import Clckwrks.Rebac.API          (updateSchema)
import Clckwrks.Rebac.Acid         (GetSchema(..), GetSchemaById(..), UpdateSchema(..),  UpdateSchemaResult(..), UpdateSchemaError(..))
import Clckwrks.Rebac.Types        (SchemaId(..), SchemaText(..))
import Clckwrks.Rebac.URL          (RebacURL(..))
import Clckwrks.Unauthorized       (unauthorizedPage)
import Control.Monad.State         (get)
import Control.Monad.Trans         (liftIO)
import qualified Data.Acid         as Acid
import Data.Text                   (pack)
import qualified Data.Text         as Text
import Data.Text.Lazy              (Text)
import Data.Maybe                  (fromMaybe)
import Data.UserId                 (UserId)
import Happstack.Authenticate.Core (Email(..), User(..))
import Happstack.Authenticate.Handlers (GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), (<++), mapView, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, setAttrs, textarea)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)

-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

viewSchema :: RebacURL -> SchemaId -> Clck RebacURL Response
viewSchema here sid =
  do mSchemaText <- query (GetSchemaById sid)
     case mSchemaText of
       Nothing ->
         template "REBAC Schema"  () $
          [hsx|
               <pre><code><% show sid %> not found.</code></pre>
          |]
       (Just (SchemaText schemaTxt)) ->
         template "REBAC Schema"  () $
          [hsx|
               <pre><code><% schemaTxt %></code></pre>
          |]

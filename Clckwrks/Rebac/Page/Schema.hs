{-# LANGUAGE FlexibleContexts, RecordWildCards, OverloadedStrings, QuasiQuotes, TypeFamilies #-}
module Clckwrks.Rebac.Page.Schema where

import AccessControl.Schema          (KnownPermission, Permission(..), Schema(..), ToPermission(..), parseSchema, ppSchema)
import AccessControl.Relation        (ToObject(..), Object(..), ObjectId(..), ObjectType(..), Relation(..), ToRelation(..), ppRelationTuples)
import Clckwrks
import Clckwrks.Monad              (plugins)
import Clckwrks.Admin.Template     (template)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..))
import Clckwrks.Rebac.API          (checkSchema, updateSchema)
import Clckwrks.Rebac.Acid         (GetSchema(..), UpdateSchema(..),  UpdateSchemaResult(..), UpdateSchemaError(..), ppUpdateSchemaError)
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

schemaPanel :: RebacURL -> Clck RebacURL Response
schemaPanel here =
  do (SchemaText schemaTxt) <- query GetSchema
     template "REBAC Schema"  () $ [hsx|
      <% reform (form ("" :: String)) "update-schema" updated Nothing (schemaForm schemaTxt) %>
     |]
       where
         schemaForm :: Text.Text -> ClckForm RebacURL (Text.Text, Text.Text)
         schemaForm schemaTxt =
           errorList ++>
           (((,) <$>
             (divControlGroup $
              divControls $
              ((textarea 200 50 schemaTxt) `setAttrs` [("class" := "span12") :: Attr Text Text])) <*>
             (divControlGroup $
              divControls $
              ((textarea 200 1 "") `setAttrs` [("class" := "span12") :: Attr Text Text])) <*
            (divControlGroup $
              divControls $
               inputSubmit "update")) `transformEitherM` parseAndValidate)

         divControlGroup :: ClckForm url a -> ClckForm url a
         divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])

         divControls :: ClckForm url a -> ClckForm url a
         divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])

         parseAndValidate :: (Text.Text, Text.Text) -> Clck RebacURL (Either ClckFormError (Text.Text, Text.Text))
         parseAndValidate (schemaTxt, comment) =
           do r <- checkSchema (SchemaText schemaTxt) comment
              case r of
                Nothing -> pure (Right (schemaTxt, comment))
                (Just err) ->
                  pure (Left (UpdateSchemaError err))

{-
           case parseSchema schemaTxt of
             (Left e) -> pure (Left (UpdateSchemaError (SchemaParseError e)))
             (Right schema) ->
               do r <- updateSchema (SchemaText schema)
                  case r of
                    SchemaUpdated -> pure (Right schema)
-}

         updated :: (Text.Text, Text.Text) -> Clck RebacURL Response
         updated (schemaText,  updateComment) =
           do r <- updateSchema (SchemaText schemaText) updateComment
              case r of
                (SchemaUpdated sid) ->
                  seeOtherURL SchemaPanel
                (SchemaUpdateFailed err) ->
                  template "REBAC Schema Update Failed"  () [hsx|
                   <div>
                     <h1>Schema Update Failure</h1>
                     <p>An error was encountered while updating the schema.</p>
                     <pre><% show $ ppUpdateSchemaError err %></pre>
                   </div> |]

{-
              <%>
               <pre><code><% show $ ppSchema schema %></code></pre>
              </%>
-}

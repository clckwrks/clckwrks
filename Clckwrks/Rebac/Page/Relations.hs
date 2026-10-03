{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, FlexibleContexts, GADTs, DataKinds #-}
module Clckwrks.Rebac.Page.Relations where

import AccessControl.Schema          (KnownPermission, Permission(..), ToPermission(..), Schema(..), parseSchema, ppSchema, knownObjectTypes, knownRelations)
import AccessControl.Relation        (Expiration, Object(..), ObjectId(..), ObjectType(..), ObjectWildcard(..), Relation(..), RelationTuple(..), ToObject(..), ToRelation(..), WildcardObjectId(..), Tag(..), formatExpiration, parseExpiration, ppRelationTuple, ppRelationTuples)
import Clckwrks
import Clckwrks.AccessControl      (checkAccess)
import Clckwrks.Monad              (ClckFormError(AddRelationTupleError, InvalidISO8601Date), plugins)
import Clckwrks.Admin.Template     (template)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.Authenticate.Monad (AuthenticatePluginState(..))
import Clckwrks.ProfileData.Acid   (GetProfileData(..), SetProfileData(..))
import Clckwrks.Rebac.Acid         (AddRelationTuple(..), RelationLogEntry(..), RemoveRelationTuple(..))
import Clckwrks.Rebac.API          (RebacApi(..), addRelationTuple, removeRelationTuple, getRelationTuples, getSchema)
import Clckwrks.Rebac.Types        (SchemaText(..))
import Clckwrks.Rebac.URL          (RebacURL(..))
import Clckwrks.Unauthorized       (unauthorizedPage)
import Control.Monad.State         (get)
import Control.Monad.Trans         (liftIO)
import qualified Data.Acid         as Acid
import Data.Text                   (pack)
import qualified Data.Text         as Text
import Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy    as TL
import Data.Time.Clock.POSIX       (getPOSIXTime)
import Data.Maybe                  (fromMaybe)
import Data.UserId                 (UserId)
import Happstack.Authenticate.Core (Email(..), User(..))
import Happstack.Authenticate.Handlers (GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), (<++), mapView, transformEither, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, select, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)

-- FIXME: the input fields allow arbitrary text even though ids and other values are restricted in what can actually appear. See the pName parser in the rebac library.
-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

-- FIXME: should this call a varient of `getRelationTuples` where we see that the user is allowed to view them?
relationsPanel :: RebacURL -> Clck RebacURL Response
relationsPanel here =
  do ets    <- getRelationTuples
     (knownObjectTys, knownRels, knownSubjectRels) <-
       do eSchemaText <- getSchema
          case eSchemaText of
            (Left e)  -> pure ([], [], [])
            (Right (SchemaText st)) ->
              case parseSchema st of
                (Right s) -> pure (knownObjectTypes s, knownRelations s, [ Relation "membership" ]) -- FIXME: should be calculated. Also it is called a subject relation, but it seems like it is a permission?
     now <- liftIO getPOSIXTime
     canMod <- checkAccess RelationsR (Permission "modify") (Just now)
     action <- showURL here
     case ets of
       (Right tuples) ->
         do template "REBAC Relations"  () $ [hsx|
              <%>
               <% relationsTable action knownObjectTys knownRels knownSubjectRels tuples %>
               <% show canMod %>
              </%>
                                              |]
       (Left e) ->
         unauthorizedPage ("You do not have permission to view the relation tuples" :: Text)
--         ok $ toResponse $ show e

--                <pre><code><% show $ ppRelationTuples tuples %></code></pre>

inputText' txt = inputText txt `setAttrs` [("size" := "15") :: Attr Text Text]

objectFormletNoWildcard :: [ ObjectType ] -> ClckForm RebacURL (Object NoWildcard)
objectFormletNoWildcard objectTypes =
--  Object <$> (td $ ObjectType <$> inputText' "") <*> (td $ ObjectId <$> inputText' "")
  Object <$> (td $ select ((ObjectType "","") : (map (\ot@(ObjectType ott) -> (ot, ott)) objectTypes)) ((==) (ObjectType ""))) <*> (td $ ObjectId <$> inputText' "")
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])

objectFormletAllowWildcard :: [ ObjectType ] -> ClckForm RebacURL (Object AllowWildcard)
objectFormletAllowWildcard objectTypes =
--  Object <$> (td $ ObjectType <$> inputText' "") <*> (td $ ObjectId <$> inputText' "")
  Object <$> (td $ select ((ObjectType "","") : (map (\ot@(ObjectType ott) -> (ot, ott)) objectTypes)) ((==) (ObjectType ""))) <*> (td $ Specific . ObjectId <$> inputText' "")
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])
    toSubjectId txt
      | txt == "*" = Wildcard
      | otherwise  = Specific (ObjectId txt)

tagFormlet :: ClckForm RebacURL (Maybe Tag)
tagFormlet =
  td $ fmap toTag (inputText' "")
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])
    toTag txt
      | Text.null txt = Nothing
      | otherwise  = Just (Tag txt)


emptyRelationIsNothing :: Relation -> Maybe Relation
emptyRelationIsNothing r@(Relation txt)
  | Text.null txt = Nothing
  | otherwise  = Just r

data FormAction
  = AddRT RelationTuple
  | RemoveRT RelationTuple


expirationFormlet :: ClckForm RebacURL (Maybe Expiration)
expirationFormlet =
  td $ transformEither (inputText ("" :: Text.Text)) (\t ->
                                                        if Text.null t
                                                        then Right Nothing
                                                        else case parseExpiration (Text.unpack t) of
                                                               Nothing -> Left (InvalidISO8601Date t)
                                                               (Just e) -> Right (Just e))
  where
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])

relationTupleFormlet :: [ ObjectType ] -> [ Relation ] -> [ Relation ] -> ClckForm RebacURL FormAction
relationTupleFormlet knownObjectTys knownRels knownSubjectRels =
  (tr ((AddRT <$> (RelationTuple
                      <$> (objectFormletNoWildcard knownObjectTys)
                      <*> (td $ select ((Relation "","") : (map (\r@(Relation rTxt) -> (r,rTxt)) knownRels)) ((==) (Relation "")))
                      <*> (objectFormletAllowWildcard knownObjectTys)
                      <*> (td $ (fmap emptyRelationIsNothing $ select ((Relation "","") : (map (\r@(Relation rTxt) -> (r,rTxt)) knownSubjectRels)) ((==) (Relation ""))))
                      <*> tagFormlet
                      <*> expirationFormlet
                      <* (td $ inputSubmit "+"))))) <++ (errorRow $ errorList)
  where
    tr = mapView (\xml -> [[hsx|<tr><% xml %></tr>|]])
    td = mapView (\xml -> [[hsx|<td><% xml %></td>|]])
    errorRow = mapView (\xml -> [[hsx|<tr><td colspan="8"><% xml %></td></tr>|]])
  {-
    (fieldset $
      (divControlGroup $
        (divControls (label' "new password" ++> inputPassword))
        )
      <* (divControlGroup $ divControls $ inputSubmit "Change Password"  `setAttrs` [("class" := "btn") :: Attr Text Text])
      ) `transformEitherM` updatePassword
    where
      label' :: Text -> ClckForm ProfileDataURL ()
      label' str      = (labelText str `setAttrs` [("class":="control-label") :: Attr Text Text])

      divControlGroup = mapView (\xml -> [[hsx|<div class="control-group"><% xml %></div>|]])
      divControls     = mapView (\xml -> [[hsx|<div class="controls"><% xml %></div>|]])
-}
relationsTable action knownObjectTys knownRels knownSubjectRels tuples =
  [hsx|
     <div>
      <table class="rebac-relations">
       <caption>Relations</caption>
       <thead>
        <tr>
         <th colspan="2">Resource</th>
         <th>Relation</th>
         <th colspan="3">Subject</th>
         <th>Tag</th>
         <th>Expiration</th>
        </tr>
        <tr>
         <th>Object Type</th>
         <th>Object ID</th>
         <th></th>
         <th>Object Type</th>
         <th>Object ID</th>
         <th>Subject Relation</th>
         <th></th>
        </tr>
       </thead>
       <tbody>
        <% mapM mkRow (zip [0..] tuples) %>
        <% reform (form ("" :: String)) "rebac" updated Nothing ((relationTupleFormlet knownObjectTys knownRels knownSubjectRels) `transformEitherM` tryUpdate) %>
       </tbody>
      </table>

     </div>
      |]
    where
      tryUpdate :: FormAction -> Clck RebacURL (Either ClckFormError RelationLogEntry)
      tryUpdate (AddRT rt) =
        do userid <- whoami
           r <- addRelationTuple rt "Added directly by admin"
           case r of
             (Right rle) -> pure (Right rle)
             (Left e)    -> pure (Left (AddRelationTupleError e))

      updated :: RelationLogEntry -> Clck RebacURL Response
      updated rle =
        seeOtherURL RelationsPanel

      deleteAction (RemoveRT rt) =
        do -- liftIO $ putStrLn $ "RemoveRT - " ++ show (ppRelationTuple rt)
           userid <- whoami
           removeRelationTuple rt "Removed directly by admin"
           seeOtherURL RelationsPanel

{-
      updated :: FormAction -> Clck RebacURL Response
      updated (AddRT rt) =
        do userid <- whoami
           r <- addRelationTuple rt "Added directly by admin"
           case r of
             (Right _) -> 
           seeOtherURL RelationsPanel
      updated (RemoveRT rt) =
        do -- liftIO $ putStrLn $ "RemoveRT - " ++ show (ppRelationTuple rt)
           userid <- whoami
           removeRelationTuple rt "Removed directly by admin"
           seeOtherURL RelationsPanel
-}
{-
      mkRow :: ( StringType (ServerPartT IO) ~ Text
               , XMLType (ServerPartT IO) ~ XML
               ) => RelationTuple -> Clck RebacURL [XMLGenT (ServerPartT IO) XML]
-}
      mkRow (i, r) =
        reform (form ("" :: String)) (TL.pack $ "rebac-" ++ show i) deleteAction Nothing (fmap (\_ -> (RemoveRT r)) ((mapView (\x -> [mkRow'' r x]) ((inputSubmit "X") :: ClckForm RebacURL (Maybe Text.Text)))))
      mkRow'' :: RelationTuple
              -> [XMLGenT (ClckT RebacURL (ServerPartT IO)) XML]
              -> XMLGenT (ClckT RebacURL (ServerPartT IO)) (XMLType (ClckT RebacURL (ServerPartT IO)))
      mkRow'' (RelationTuple (Object (ObjectType rt) (ObjectId ri)) (Relation r) (Object (ObjectType st) wsi) msr mTag mExpiration) delButton =
         let sr = case msr of
                    Nothing -> ""
                    (Just (Relation r)) -> r
             si = case wsi of
               Wildcard -> "*"
               (Specific (ObjectId i)) -> i
         in
             [hsx|
                    <tr><td><% rt  %></td>
                        <td><% ri  %></td>
                        <td><% r   %></td>
                        <td><% st  %></td>
                        <td><% si  %></td>
                        <td><% sr %></td>
                        <td><% maybe "" unTag mTag %></td>
                        <td><% maybe "" formatExpiration mExpiration %></td>
                        <td><% delButton %></td>
                    </tr>
                    |]
{-
      mkRow' (RelationTuple (Object (ObjectType rt) (ObjectId ri)) (Relation r) (Object (ObjectType st) (ObjectId si)) msr )  =
         let sr = case msr of
                    Nothing -> ""
                    (Just (Relation r)) -> r
         in
             [hsx|
                    <tr><td><% rt  %></td>
                        <td><% ri  %></td>
                        <td><% r   %></td>
                        <td><% st  %></td>
                        <td><% si  %></td>
                        <td><% sr %></td>
                        <td><button>X</button></td>
                    </tr>
                    |]
-}

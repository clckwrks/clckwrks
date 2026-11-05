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
import Clckwrks.Rebac.Acid         (AddRelationTuple(..), RelationLogEntry(..), RemoveRelationTuple(..), TupleFilter(..), anyTuple)
import Clckwrks.Rebac.API          (RebacApi(..), addRelationTuple, removeRelationTuple, getRelationTuplesPage, getSchema)
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
import Happstack.Server            (getDataFn, lookRead, lookText')
import Happstack.Authenticate.Handlers (GetUserByUserId(..), UpdateUser(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform                 ((++>), (<++), mapView, transformEither, transformEitherM)
import Text.Reform.HSP.Text        (form, inputText, inputSubmit, labelText, fieldset, ol, li, errorList, select, setAttrs)
import Text.Reform.Happstack       (reform)
import HSP.XMLGenerator
import HSP.XML
import Web.Plugins.Core            (Plugin(..), getPluginState)
import Web.Routes                  (showURLParams)

-- FIXME: the input fields allow arbitrary text even though ids and other values are restricted in what can actually appear. See the pName parser in the rebac library.
-- FIXME: this currently uses the admin template. Which is sort of right, and sort of not.

-- | The relation tuples, a page at a time, filtered by the query string:
-- @rtype@, @rid@, @rel@, @stype@, @sid@ and @tag@ (see 'TupleFilter'),
-- and @page@, counting from 1.  The add and delete forms post to the same
-- URL, so they see the view too and go back to it.
relationsPanel :: RebacURL -> Clck RebacURL Response
relationsPanel here =
  do (tf, page) <- tupleView
     let offset = (page - 1) * relationsPageSize
     ets    <- getRelationTuplesPage tf offset relationsPageSize
     (knownObjectTys, knownRels, knownSubjectRels) <-
       do eSchemaText <- getSchema
          case eSchemaText of
            (Left e)  -> pure ([], [], [])
            (Right (SchemaText st)) ->
              case parseSchema st of
                (Right s) -> pure (knownObjectTypes s, knownRelations s, [ Relation "member" ]) -- FIXME: should be calculated. Also it is called a subject relation, but it seems like it is a permission?
     now <- liftIO getPOSIXTime
     canMod <- checkAccess RelationsR (Permission "modify") (Just now)
     case ets of
       (Right (total, tuples)) ->
         do let viewURL f p = showURLParams here (viewParams f p)
            back  <- viewURL tf page
            clear <- showURL here
            prev  <- if page > 1 then Just <$> viewURL tf (page - 1) else pure Nothing
            next  <- if offset + relationsPageSize < total then Just <$> viewURL tf (page + 1) else pure Nothing
            let link (Just u) label = [hsx| <a href=u><% label %></a> |]
                link Nothing  label = [hsx| <span><% label %></span> |]
                range | null tuples = "no relations match"
                      | otherwise = "relations " ++ show (offset + 1) ++ " to " ++ show (offset + length tuples) ++ " of " ++ show total
                nav = [hsx| <p class="rebac-relations-pages"><% link prev ("« Previous" :: Text) %> · <% range %> · <% link next ("Next »" :: Text) %></p> |]
            template "REBAC Relations"  () $ [hsx|
              <%>
               <% filterForm clear knownObjectTys knownRels tf %>
               <% nav %>
               <% relationsTable back (\f -> viewURL f 1) knownObjectTys knownRels knownSubjectRels tuples %>
               <% nav %>
               <% show canMod %>
              </%>
                                              |]
       (Left e) ->
         unauthorizedPage ("You do not have permission to view the relation tuples" :: Text)
--         ok $ toResponse $ show e

--                <pre><code><% show $ ppRelationTuples tuples %></code></pre>

-- | how many relations a page shows
relationsPageSize :: Int
relationsPageSize = 100

-- | The filter and page from the query string.  Blank fields are left
-- out of the filter; a missing or unreadable page is page 1.
tupleView :: Clck RebacURL (TupleFilter, Int)
tupleView =
  do let param name = either (const Nothing) nonBlank <$> getDataFn (lookText' name)
         nonBlank t = let t' = Text.strip t in if Text.null t' then Nothing else Just t'
     tf <- TupleFilter <$> (fmap ObjectType <$> param "rtype")
                       <*> param "rid"
                       <*> (fmap Relation <$> param "rel")
                       <*> (fmap ObjectType <$> param "stype")
                       <*> param "sid"
                       <*> param "tag"
     page <- either (const 1) (max 1) <$> getDataFn (lookRead "page")
     pure (tf, page)

-- | The query string for a filter and page, the inverse of 'tupleView'.
viewParams :: TupleFilter -> Int -> [(Text.Text, Maybe Text.Text)]
viewParams tf page =
  [ (name, Just v) | (name, Just v) <- [ ("rtype", unObjectType <$> tfResourceType tf)
                                       , ("rid",   tfResourceId tf)
                                       , ("rel",   unRelation <$> tfRelation tf)
                                       , ("stype", unObjectType <$> tfSubjectType tf)
                                       , ("sid",   tfSubjectId tf)
                                       , ("tag",   tfTag tf) ] ] ++
  [ ("page", Just (pack (show page))) | page > 1 ]

-- | The filter, as a GET form, so that a filtered view is a URL that can
-- be bookmarked and that the page links keep.  Ids and tags match
-- exactly, or as a pattern with @*@; see 'TupleFilter'.
filterForm clear knownObjectTys knownRels tf =
  [hsx|
    <form method="get" action="" class="rebac-relations-filter">
     <div class="rebac-filter-field">
      <label for="rebac-filter-rtype">Resource</label>
      <div class="rebac-filter-inputs">
       <% typeSelect "rtype" (tfResourceType tf) %>
       <% textInput "rid" "id, or 3f2a*" (tfResourceId tf) %>
      </div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-filter-rel">Relation</label>
      <div class="rebac-filter-inputs"><% relationSelect (tfRelation tf) %></div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-filter-stype">Subject</label>
      <div class="rebac-filter-inputs">
       <% typeSelect "stype" (tfSubjectType tf) %>
       <% textInput "sid" "id, or 12*" (tfSubjectId tf) %>
      </div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-filter-tag">Tag</label>
      <div class="rebac-filter-inputs"><% textInput "tag" "tag, or report:*" (tfTag tf) %></div>
     </div>
     <div class="rebac-filter-actions">
      <button type="submit" class="btn btn-primary">Filter</button>
      <a class="btn" href=clear>Clear</a>
     </div>
    </form>
  |]
  where
    typeSelect :: Text.Text -> Maybe ObjectType -> GenXML (Clck RebacURL)
    typeSelect name cur =
      [hsx| <select name=name id=("rebac-filter-" <> name)><% option "" "any" (cur == Nothing) : [ option t t (cur == Just ot) | ot@(ObjectType t) <- knownObjectTys ] %></select> |]
    relationSelect :: Maybe Relation -> GenXML (Clck RebacURL)
    relationSelect cur =
      [hsx| <select name="rel" id="rebac-filter-rel"><% option "" "any" (cur == Nothing) : [ option t t (cur == Just r) | r@(Relation t) <- knownRels ] %></select> |]
    option :: Text.Text -> Text.Text -> Bool -> GenXML (Clck RebacURL)
    option v label True  = [hsx| <option value=v selected="selected"><% label %></option> |]
    option v label False = [hsx| <option value=v><% label %></option> |]
    textInput :: Text.Text -> Text.Text -> Maybe Text.Text -> GenXML (Clck RebacURL)
    textInput name hint cur = [hsx| <input type="text" name=name id=("rebac-filter-" <> name) placeholder=hint value=(fromMaybe "" cur) /> |]

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
relationsTable back filterURL knownObjectTys knownRels knownSubjectRels tuples =
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
        <% reform (form ("" :: String)) "rebac" updated Nothing ((relationTupleFormlet knownObjectTys knownRels knownSubjectRels) `transformEitherM` tryUpdate) %>
        <% mapM mkRow (zip [0..] tuples) %>
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

      -- back to the same filter and page
      updated :: RelationLogEntry -> Clck RebacURL Response
      updated rle =
        seeOther back (toResponse ())

      deleteAction (RemoveRT rt) =
        do -- liftIO $ putStrLn $ "RemoveRT - " ++ show (ppRelationTuple rt)
           userid <- whoami
           removeRelationTuple rt "Removed directly by admin"
           seeOther back (toResponse ())

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
             -- each links to the relations with the same resource,
             -- subject or tag
             filterLink f label = do u <- XMLGenT (filterURL f)
                                     [hsx| <a href=u><% label %></a> |]
             -- the same tuple asked of the Check panel: does the subject
             -- have the relation, and why
             checkLink = do u <- XMLGenT (showURLParams CheckPanel [ ("rtype", Just rt), ("rid", Just ri), ("perm", Just r)
                                                                  , ("stype", Just st), ("sid", Just si) ])
                            [hsx| <a href=u class="rebac-relations-check">check</a> |]
             tagCell = case mTag of
                         Nothing      -> [hsx| <span></span> |]
                         Just (Tag t) -> filterLink anyTuple { tfTag = Just t } t
         in
             [hsx|
                    <tr><td><% rt  %></td>
                        <td><% filterLink anyTuple { tfResourceType = Just (ObjectType rt), tfResourceId = Just ri } ri %></td>
                        <td><% r   %></td>
                        <td><% st  %></td>
                        <td><% filterLink anyTuple { tfSubjectType = Just (ObjectType st), tfSubjectId = Just si } si %></td>
                        <td><% sr %></td>
                        <td><% tagCell %></td>
                        <td><% maybe "" formatExpiration mExpiration %></td>
                        <td><% delButton %> <% checkLink %></td>
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

{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | The admin Check panel: ask whether a subject has a permission on a
-- resource and see why, as rebac evaluated it ('explainPermission'), or
-- which resources of a type a subject has a permission on
-- ('lookupResources').
--
-- Everything is in the query string, so a check is a URL:
--
-- * @tab@: @check@ (the default) or @lookup@
-- * @rtype@, @rid@: the resource (only the type, for a lookup)
-- * @perm@: the permission or relation
-- * @stype@, @sid@: the subject
-- * @at@: the time to check at, ISO 8601; blank is now
-- * @every@: @1@ to evaluate every path, not just what the check needed
-- * @view@: @tree@ (the default) or @paths@
module Clckwrks.Rebac.Page.Check
  ( checkPanel
  ) where

import AccessControl.Explain       (Explanation(..), ExprExplanation(..), ExprHow(..), How(..), RelationExplanation(..), grantingPaths)
import AccessControl.Relation      (Object(..), ObjectId(..), ObjectType(..), ObjectWildcard(..), Relation(..), RelationTuple(..), WildcardObjectId(..), formatExpiration, ppObject, ppRelationTuple)
import AccessControl.Schema        (Definition(..), ObjectPermission(..), ObjectRelation(..), Permission(..), PermissionExpression, Schema(..), parseSchema, ppPermissionExpression)
import Clckwrks
import Clckwrks.Admin.Template     (template)
import Clckwrks.Rebac.Acid         (ExplainedCheck(..), RelationLogEntry(..), RelationTxId(..))
import Clckwrks.Rebac.API          (explainPermission, getSchema, lookupResources)
import Clckwrks.Rebac.Types        (SchemaId(..), SchemaText(..))
import Clckwrks.Rebac.URL          (RebacURL(..))
import Clckwrks.Unauthorized       (unauthorizedPage)
import Control.Monad               (forM)
import Control.Monad.Trans         (liftIO)
import qualified Data.Map          as Map
import Data.Maybe                  (fromMaybe)
import Data.Text                   (Text)
import qualified Data.Text         as Text
import qualified Data.Text.Lazy    as TL
import Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601    (iso8601ParseM)
import Happstack.Server            (getDataFn, lookText')
import HSP.XMLGenerator
import HSP.XML
import Language.Haskell.HSX.QQ     (hsx)
import Web.Routes                  (showURLParams)

-- | What the query string asks.
data Ask = Ask
  { askTab   :: Text
  , askRType :: Maybe Text
  , askRId   :: Maybe Text
  , askPerm  :: Maybe Text
  , askSType :: Maybe Text
  , askSId   :: Maybe Text
  , askAt    :: Maybe Text
  , askEvery :: Bool
  , askView  :: Text
  }

readAsk :: Clck RebacURL Ask
readAsk =
  do let param name = either (const Nothing) nonBlank <$> getDataFn (lookText' name)
         nonBlank t = let t' = Text.strip t in if Text.null t' then Nothing else Just t'
     Ask <$> (fromMaybe "check" <$> param "tab")
         <*> param "rtype" <*> param "rid" <*> param "perm"
         <*> param "stype" <*> param "sid" <*> param "at"
         <*> ((== Just "1") <$> param "every")
         <*> (fromMaybe "tree" <$> param "view")

-- | The query string for an 'Ask', the inverse of 'readAsk'.
askParams :: Ask -> [(Text, Maybe Text)]
askParams Ask{..} =
  [ (n, Just v) | (n, Just v) <- [ ("tab", Just askTab), ("rtype", askRType), ("rid", askRId), ("perm", askPerm)
                                 , ("stype", askSType), ("sid", askSId), ("at", askAt)
                                 , ("every", if askEvery then Just "1" else Nothing), ("view", Just askView) ] ]

checkPanel :: RebacURL -> Clck RebacURL Response
checkPanel here =
  do q <- readAsk
     (types, names) <- schemaNames
     now <- liftIO getPOSIXTime
     let at = case askAt q of
                Nothing -> Right now
                Just t  -> maybe (Left t) (Right . utcTimeToPOSIXSeconds) (iso8601ParseM (Text.unpack t))
         askURL q' = showURLParams here (askParams q')
     checkTab  <- askURL q { askTab = "check" }
     lookupTab <- askURL q { askTab = "lookup" }
     result <-
       case (askTab q, at) of
         (_, Left t) -> pure (Right [hsx| <p class="rebac-check-error">Not an ISO 8601 time: <% t %></p> |])
         ("lookup", Right t) -> lookupResult askURL q t
         (_, Right t) -> checkResult askURL q t
     case result of
       Left e -> unauthorizedPage ("You do not have permission to check permissions: " <> TL.fromStrict e)
       Right body ->
         template "REBAC Check" () $ [hsx|
           <%>
            <p class="rebac-check-tabs">
             <% tabLink (askTab q /= "lookup") checkTab "Check" %>
             <% tabLink (askTab q == "lookup") lookupTab "Lookup" %>
            </p>
            <% checkForm q types names %>
            <% body %>
           </%> |]
  where
    tabLink :: Bool -> Text -> Text -> GenXML (Clck RebacURL)
    tabLink True  _ label = [hsx| <span class="rebac-check-tab-current"><% label %></span> |]
    tabLink False u label = [hsx| <a class="rebac-check-tab" href=u><% label %></a> |]

-- | The object types, and each type's permissions and relations, from the
-- current schema, for the form's choices.
schemaNames :: Clck RebacURL ([Text], [(Text, [Text])])
schemaNames =
  do es <- getSchema
     pure $ case es of
       Right (SchemaText st) | Right (Schema defs) <- parseSchema st ->
         ( map defName defs
         , [ (defName d, [ opName p | Right p <- defDecls d ] ++ [ orName r | Left r <- defDecls d ]) | d <- defs ] )
       _ -> ([], [])

checkForm :: Ask -> [Text] -> [(Text, [Text])] -> GenXML (Clck RebacURL)
checkForm q types names =
  [hsx|
    <form method="get" action="" class="rebac-relations-filter">
     <input type="hidden" name="tab" value=(askTab q) />
     <input type="hidden" name="view" value=(askView q) />
     <div class="rebac-filter-field">
      <label for="rebac-check-rtype">Resource</label>
      <div class="rebac-filter-inputs">
       <% typeSelect "rtype" (askRType q) %>
       <% if askTab q == "lookup" then anyId else textInput "rid" "id" (askRId q) %>
      </div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-check-perm">Permission or relation</label>
      <div class="rebac-filter-inputs">
       <select name="perm" id="rebac-check-perm">
        <% option "" "" (askPerm q == Nothing) %>
        <% map optgroup names %>
       </select>
      </div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-check-stype">Subject</label>
      <div class="rebac-filter-inputs">
       <% typeSelect "stype" (Just (fromMaybe "user" (askSType q))) %>
       <% textInput "sid" "id" (askSId q) %>
      </div>
     </div>
     <div class="rebac-filter-field">
      <label for="rebac-check-at">At</label>
      <div class="rebac-filter-inputs"><% textInput "at" "now" (askAt q) %></div>
     </div>
     <% if askTab q == "lookup" then nothing else everyBox %>
     <div class="rebac-filter-actions">
      <button type="submit" class="btn btn-primary"><% if askTab q == "lookup" then "Look up" else "Check" :: Text %></button>
     </div>
    </form>
  |]
  where
    -- the same name can be in more than one type; select it under the
    -- resource's type when that is known
    sameType ty = maybe True (== ty) (askRType q)
    anyId = [hsx| <span class="rebac-check-note">any id</span> |]
    optgroup (ty, ns) = [hsx| <optgroup label=ty><% [ option n n (askPerm q == Just n && sameType ty) | n <- ns ] %></optgroup> |]
    everyBox =
      [hsx| <div class="rebac-filter-field">
             <label class="rebac-check-every">
              <% everyInput %>
              Show every path, not just what the check needed
             </label>
            </div> |]
    everyInput
      | askEvery q = [hsx| <input type="checkbox" name="every" value="1" checked="checked" /> |]
      | otherwise  = [hsx| <input type="checkbox" name="every" value="1" /> |]
    typeSelect :: Text -> Maybe Text -> GenXML (Clck RebacURL)
    typeSelect name cur =
      [hsx| <select name=name id=("rebac-check-" <> name)><% option "" "" (cur == Nothing) : [ option t t (cur == Just t) | t <- types ] %></select> |]
    option :: Text -> Text -> Bool -> GenXML (Clck RebacURL)
    option v label True  = [hsx| <option value=v selected="selected"><% label %></option> |]
    option v label False = [hsx| <option value=v><% label %></option> |]
    textInput :: Text -> Text -> Maybe Text -> GenXML (Clck RebacURL)
    textInput name hint cur = [hsx| <input type="text" name=name id=("rebac-check-" <> name) placeholder=hint value=(fromMaybe "" cur) /> |]

-- * Check

checkResult :: (Ask -> Clck RebacURL Text) -> Ask -> POSIXTime -> Clck RebacURL (Either Text (GenXML (Clck RebacURL)))
checkResult askURL q at =
  case (askRType q, askRId q, askPerm q, askSType q, askSId q) of
    (Just rty, Just rid, Just perm, Just sty, Just sid) ->
      do let res  = Object (ObjectType rty) (ObjectId rid)
             subj = Object (ObjectType sty) (ObjectId sid)
         er <- explainPermission (askEvery q) res (Permission perm) subj (Just at)
         case er of
           Left e -> pure (Left e)
           Right ec ->
             do treeURL  <- askURL q { askView = "tree" }
                pathsURL <- askURL q { askView = "paths" }
                relsURL  <- showURLParams RelationsPanel [("rtype", Just rty), ("rid", Just rid)]
                schemaURL <- maybe (pure Nothing) (fmap Just . showURL . ViewSchema) (ecSchemaId ec)
                pure $ Right (explained q ec res perm subj treeURL pathsURL relsURL schemaURL)
    _ -> pure (Right [hsx| <p class="rebac-check-note">Choose a resource, a permission or relation, and a subject.</p> |])

explained :: Ask -> ExplainedCheck -> Object 'NoWildcard -> Text -> Object 'NoWildcard -> Text -> Text -> Text -> Maybe Text -> GenXML (Clck RebacURL)
explained q ec res perm subj treeURL pathsURL relsURL schemaURL =
  [hsx|
    <div class="rebac-check-result">
     <p class=(if exAllowed e then "rebac-check-verdict rebac-allowed" else "rebac-check-verdict rebac-denied" :: Text)>
      <span class="rebac-check-verdict-word"><% if exAllowed e then "✔ ALLOWED" else "✘ DENIED" :: Text %></span>
      <% " — " :: Text %>
      <span class="rebac-check-question"><% obj subj %> has <b><% perm %></b> on <% obj res %></span>
     </p>
     <p class="rebac-check-views">
      <% viewLink (askView q /= "paths") treeURL "Tree" %> ·
      <% viewLink (askView q == "paths") pathsURL "Ways it is allowed" %>
     </p>
     <% if askView q == "paths" then pathsView else treeView %>
     <p class="rebac-check-footer">
      <% schemaLink %>
      · <a href=relsURL>Relations of <% obj res %></a>
     </p>
    </div>
  |]
  where
    e = ecExplanation ec
    viewLink :: Bool -> Text -> Text -> GenXML (Clck RebacURL)
    viewLink True  _ label = [hsx| <b><% label %></b> |]
    viewLink False u label = [hsx| <a href=u><% label %></a> |]
    schemaLink =
      case (schemaURL, ecSchemaId ec) of
        (Just u, Just (SchemaId n)) -> [hsx| <a href=u>Schema <% show n %></a> |]
        _ -> [hsx| <span>Schema unknown</span> |]
    treeView = [hsx| <ul class="rebac-check-tree"><% explanationNode (ecAdded ec) e %></ul> |]
    pathsView =
      case grantingPaths e of
        [] -> [hsx| <p class="rebac-check-note">No way: the subject is not allowed. The tree shows every reason.</p> |]
        ps -> [hsx|
                <div>
                 <ol class="rebac-check-paths"><% map pathItem ps %></ol>
                 <% if askEvery q then nothing else onlyFound %>
                </div> |]
    pathItem p = [hsx| <li><% map pathStep p %></li> |]
    pathStep rt = [hsx| <div><% tupleLine (ecAdded ec) rt %></div> |]
    onlyFound = [hsx| <p class="rebac-check-note">Only the way the check found; tick "Show every path" for all of them.</p> |]

-- | One name on one resource, and how it was decided.
explanationNode :: Map.Map RelationTuple RelationLogEntry -> Explanation -> GenXML (Clck RebacURL)
explanationNode added (Explanation res name ok how) =
  case how of
    Cycle         -> leaf [hsx| <span><% name %> on <% obj res %>: <span class="rebac-check-why">↻ already being checked further up (a cycle)</span></span> |]
    NoTuples      -> leaf [hsx| <span><% name %> on <% obj res %>: <span class="rebac-check-why">no tuples for <% obj res %></span></span> |]
    UndefinedType -> leaf [hsx| <span><% name %> on <% obj res %>: <span class="rebac-check-why">type <% unObjectType (objectType res) %> is not defined</span></span> |]
    UndefinedName -> leaf [hsx| <span><% name %> on <% obj res %>: <span class="rebac-check-why">not a permission or relation of <% unObjectType (objectType res) %></span></span> |]
    ByPermission x ->
      [hsx| <li class=(cls ok)><span class="rebac-check-rule"><% mark ok %> <% obj res %> <b><% name %></b> = <% ppExpr (eeExpr x) %></span>
              <ul><% exprChildren added x %></ul>
            </li> |]
    ByRelation r -> relationNode added (Just res) r
  where
    leaf body = [hsx| <li class=(cls ok)><% mark ok %> <% body %></li> |]

-- | The branches of a permission's expression, unions flattened.
exprChildren :: Map.Map RelationTuple RelationLogEntry -> ExprExplanation -> [GenXML (Clck RebacURL)]
exprChildren added x =
  case eeHow x of
    ExUnion l r -> exprChildren added l ++ exprChildren added r
    _           -> [exprNode added x]

exprNode :: Map.Map RelationTuple RelationLogEntry -> ExprExplanation -> GenXML (Clck RebacURL)
exprNode added x =
  case eeHow x of
    ExUnion _ _            -> [hsx| <li><ul><% exprChildren added x %></ul></li> |]
    ExRelation r           -> relationNode added Nothing r
    ExName e               -> explanationNode added e
    ExArrow []             -> [hsx| <li class=(cls False)><% mark False %> <% ppExpr (eeExpr x) %> <span class="rebac-check-why">nothing to follow</span></li> |]
    ExArrow fs             ->
      [hsx| <li class=(cls (eeAllowed x))><span class="rebac-check-rule"><% mark (eeAllowed x) %> <% ppExpr (eeExpr x) %></span>
              <ul><% [ followedNode added rt e | (rt, e) <- fs ] %></ul>
            </li> |]
    ExArrowFromNonRelation -> [hsx| <li class=(cls False)><% mark False %> <% ppExpr (eeExpr x) %> <span class="rebac-check-why">does not start from a relation</span></li> |]
    ExUnsupported          -> [hsx| <li class=(cls False)><% mark False %> <% ppExpr (eeExpr x) %> <span class="rebac-check-why">not supported</span></li> |]
    ExNotEvaluated         -> [hsx| <li class="rebac-check-skipped">– <% ppExpr (eeExpr x) %> <span class="rebac-check-why">not evaluated, already allowed</span></li> |]

-- | A relation, by its tuples.  At the top of the tree it is named with
-- its resource; inside a permission, the resource is the permission's.
relationNode :: Map.Map RelationTuple RelationLogEntry -> Maybe (Object 'NoWildcard) -> RelationExplanation -> GenXML (Clck RebacURL)
relationNode added mres rx =
  [hsx|
    <li class=(cls (reAllowed rx))>
     <span class="rebac-check-rule"><% mark (reAllowed rx) %> <% named %><% unRelation (reRelation rx) %></span>
     <ul>
      <% map matchItem (reMatches rx) %>
      <% [ followedNode added rt e | (rt, e) <- reSubjectSets rx ] %>
      <% if reAllowed rx || not (null (reSubjectSets rx)) then [] else [noTuple] %>
      <% map expiredItem (reExpired rx) %>
     </ul>
    </li>
  |]
  where
    named = maybe "" (\o -> obj o <> " ") mres :: Text
    matchItem rt = [hsx| <li class="rebac-allowed"><% tupleLine added rt %></li> |]
    expiredItem rt =
      [hsx| <li class="rebac-check-skipped">⌛ <% tupleLine added rt %> <span class="rebac-check-why">expired<% maybe "" (\x -> " " ++ formatExpiration x) (expiration rt) %>, ignored</span></li> |]
    noTuple =
      [hsx| <li class="rebac-denied">no tuple names the subject
             <% if null (reOthers rx) then "" else " (held by " <> held <> ")" :: Text %>
            </li> |]
    held = Text.intercalate ", " (map subjectText (take 5 (reOthers rx))) <> if length (reOthers rx) > 5 then ", …" else ""

-- | A subject set or arrow tuple, and the explanation on its subject.
followedNode :: Map.Map RelationTuple RelationLogEntry -> RelationTuple -> Explanation -> GenXML (Clck RebacURL)
followedNode added rt e =
  [hsx| <li class=(cls (exAllowed e))><% tupleLine added rt %> →
          <ul><% explanationNode added e %></ul>
        </li> |]

-- | A tuple, linked to the Relations panel showing it, with the log entry
-- that added it.
tupleLine :: Map.Map RelationTuple RelationLogEntry -> RelationTuple -> GenXML (Clck RebacURL)
tupleLine added rt =
  do u <- XMLGenT $ showURLParams RelationsPanel
                      [ ("rtype", Just (unObjectType (objectType (resource rt)))), ("rid", Just (unObjectId (objectId (resource rt))))
                      , ("rel", Just (unRelation (relation rt))), ("stype", Just (unObjectType (objectType (subject rt))))
                      , ("sid", Just (subjectText rt)) ]
     [hsx| <span class="rebac-check-tuple"><a href=u><code><% show (ppRelationTuple rt) %></code></a>
             <% addedNote %>
           </span> |]
  where
    addedNote =
      case Map.lookup rt added of
        Just le@RelationLogEntry{} ->
          [hsx| <span class="rebac-check-added">added <% show (rleTimestamp le) %>, tx <% show (unRelationTxId (rleTxId le)) %>, “<% rleComment le %>”</span> |]
        _ -> nothing

subjectText :: RelationTuple -> Text
subjectText rt =
  case objectId (subject rt) of
    Wildcard -> "*"
    Specific (ObjectId i) -> i

-- * Lookup

lookupResult :: (Ask -> Clck RebacURL Text) -> Ask -> POSIXTime -> Clck RebacURL (Either Text (GenXML (Clck RebacURL)))
lookupResult askURL q at =
  case (askRType q, askPerm q, askSType q, askSId q) of
    (Just rty, Just perm, Just sty, Just sid) ->
      do let subj = Object (ObjectType sty) (ObjectId sid)
         er <- lookupResources (ObjectType rty) (Permission perm) subj (Just at)
         case er of
           Left e -> pure (Left e)
           Right found ->
             do rows <- forM (take lookupLimit found) $ \o ->
                          (,) o <$> askURL q { askTab = "check", askRId = Just (unObjectId (objectId o)) }
                pure $ Right
                  [hsx|
                    <div class="rebac-check-result">
                     <p class="rebac-check-verdict">
                      <% show (length found) %> <% rty %> on which <% obj subj %> has <b><% perm %></b>
                      <% if length found > lookupLimit then " (the first " ++ show lookupLimit ++ " shown)" else "" %>
                     </p>
                     <ul class="rebac-check-lookup"><% map lookupItem rows %></ul>
                    </div> |]
    _ -> pure (Right [hsx| <p class="rebac-check-note">Choose a resource type, a permission or relation, and a subject.</p> |])

lookupItem :: (Object 'NoWildcard, Text) -> GenXML (Clck RebacURL)
lookupItem (o, u) = [hsx| <li><code><% obj o %></code> <a href=u>why?</a></li> |]

-- | how many resources a lookup lists
lookupLimit :: Int
lookupLimit = 500

-- * Pieces

nothing :: GenXML (Clck RebacURL)
nothing = [hsx| <span></span> |]

obj :: Object 'NoWildcard -> Text
obj = Text.pack . show . ppObject

ppExpr :: PermissionExpression -> Text
ppExpr = Text.pack . show . ppPermissionExpression

mark :: Bool -> Text
mark True  = "✔"
mark False = "✘"

cls :: Bool -> Text
cls True  = "rebac-allowed"
cls False = "rebac-denied"

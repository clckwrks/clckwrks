{-# LANGUAGE OverloadedStrings #-}
-- | At startup, make the stored rebac schema the one the plugins' schema
-- fragments add up to.  Plugins register their fragments with
-- 'Clckwrks.Monad.addRebacFragment' from their 'pluginInit'.
module Clckwrks.Rebac.Reconcile
  ( orderFragments
  , mergedSchema
  , reconcileRebacSchema
  ) where

import AccessControl.Schema         (Definition(..), Schema(..), SchemaError(..), ppSchemaError)
import AccessControl.SchemaFragment (SchemaFragment(..), mergeFragments)
import Clckwrks.Rebac.Acid          (GetSchema(..), RebacState, UpdateSchema(..), UpdateSchemaResult(..), ppUpdateSchemaError)
import Clckwrks.Rebac.Types         (SchemaId, SchemaText(..))
import Data.Acid                    (AcidState, query, update)
import Data.Map                     (Map)
import qualified Data.Map           as Map
import Data.Text                    (Text)
import qualified Data.Text          as T
import Data.Time.Clock              (getCurrentTime)
import Web.Plugins.Core             (PluginName)

-- | The fragments in the order their texts go into the merged schema: each
-- after the registered fragments it depends on, otherwise by plugin name,
-- so that the merged text is the same on every start.
orderFragments :: Map PluginName SchemaFragment -> [(PluginName, SchemaFragment)]
orderFragments frags = go [] (Map.toAscList frags)
  where
    registered = Map.elems frags
    go placed [] = reverse placed
    go placed pending =
      case break (ready placed) pending of
        (before, next : after) -> go (next : placed) (before ++ after)
        -- fragments cannot depend on each other in a cycle, they are
        -- built in order, but do not loop if they somehow do
        (_, []) -> reverse placed ++ pending
    ready placed (_, frag) =
      and [ dep `elem` map snd placed | dep <- fragmentDepends frag, dep `elem` registered ]

-- | The schema text the registered fragments add up to and the plugins they
-- came from, in order, or what is wrong with it, each problem labelled with
-- the plugin whose definition it is in.
mergedSchema :: Map PluginName SchemaFragment -> Either [Text] (Text, [PluginName])
mergedSchema frags =
  case mergeFragments (map snd ordered) of
    (Right (txt, _)) -> Right (txt, map fst ordered)
    (Left errs)      -> Left [ owner (seDefinition e) <> T.pack (show (ppSchemaError e)) | e <- errs ]
  where
    ordered = orderFragments frags
    owner d =
      case [ name | (name, frag) <- ordered, d `elem` map defName (definitions (fragmentSchema frag)) ] of
        []    -> ""
        names -> "plugin " <> T.intercalate ", " names <> ": "

-- | Make the stored schema the one the registered fragments add up to,
-- through 'UpdateSchema', so it is checked like any other schema update and
-- recorded in the schema history.  Returns the new 'SchemaId' if the schema
-- changed.  Does nothing if no fragment is registered.
--
-- Fails, so that the server does not start, if the fragments do not fit
-- together or the existing relation tuples are not valid under the result.
reconcileRebacSchema :: AcidState RebacState -> Map PluginName SchemaFragment -> IO (Maybe SchemaId)
reconcileRebacSchema acid frags
  | Map.null frags = pure Nothing
  | otherwise =
      case mergedSchema frags of
        (Left problems) ->
          ioError $ userError $ T.unpack $ T.unlines ("The plugins' rebac schema fragments do not fit together:" : problems)
        (Right (txt, names)) ->
          do (SchemaText current) <- query acid GetSchema
             if current == txt
               then pure Nothing
               else
                 do now <- getCurrentTime
                    r <- update acid (UpdateSchema now (SchemaText txt) ("schema fragments from plugins: " <> T.intercalate ", " names))
                    case r of
                      (SchemaUpdated sid) -> pure (Just sid)
                      (SchemaUpdateFailed err) ->
                        ioError $ userError $ "The rebac schema from the plugins' fragments was rejected:\n" ++ show (ppUpdateSchemaError err)

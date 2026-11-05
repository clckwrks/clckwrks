{-# LANGUAGE OverloadedStrings #-}
-- | At startup, make the stored rebac schema the one the plugins' schema
-- fragments add up to.  Plugins register their fragments with
-- 'Clckwrks.Monad.addRebacFragment' from their 'pluginInit'.
module Clckwrks.Rebac.Reconcile
  ( orderFragments
  , mergedSchema
  , reconcileRebacSchema
  , ReconcileResult(..)
  ) where

import AccessControl.Schema         (Definition(..), Schema(..), SchemaError(..), ppSchemaError)
import AccessControl.SchemaFragment (SchemaFragment(..), mergeFragments)
import Clckwrks.Rebac.Acid          (RebacState, ReconcileNeeded(..), ReconcileResult(..), ReconcileSchema(..), ppReconcileError)
import Clckwrks.Rebac.Types         (RebacFragment(..), SchemaText(..))
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

-- | Make the stored schema the one the registered fragments add up to, and
-- add each plugin's seed tuples that have not been seen before, in one
-- 'ReconcileSchema' transaction: the schema is checked as 'UpdateSchema'
-- would check it and recorded in the schema history, and the seed tuples are
-- checked against it.  Returns the new 'SchemaId' if the schema changed, and
-- the seed tuples that were added.  Does nothing if no fragment is
-- registered, or if nothing would change ('ReconcileNeeded'): the update
-- is written to the event log even when it changes nothing, so asking
-- first keeps a start that changes nothing from writing anything.
--
-- Fails, so that the server does not start, if the fragments do not fit
-- together, the existing relation tuples are not valid under the result, or
-- a seed tuple is not valid under it; then nothing has changed.
reconcileRebacSchema :: AcidState RebacState -> Map PluginName RebacFragment -> IO ReconcileResult
reconcileRebacSchema acid frags
  | Map.null frags = pure (ReconcileResult Nothing [])
  | otherwise =
      case mergedSchema (fmap rebacFragmentSchema frags) of
        (Left problems) ->
          ioError $ userError $ T.unpack $ T.unlines ("The plugins' rebac schema fragments do not fit together:" : problems)
        (Right (txt, names)) ->
          do let comment = "schema fragments from plugins: " <> T.intercalate ", " names
                 seeds   = [ ("seed tuples from plugin " <> name, rebacFragmentSeeds (frags Map.! name)) | name <- names ]
             needed <- query acid (ReconcileNeeded (SchemaText txt) seeds)
             if not needed
               then pure (ReconcileResult Nothing [])
               else do
                 now <- getCurrentTime
                 r <- update acid (ReconcileSchema now (SchemaText txt) comment seeds)
                 case r of
                   (Right result) -> pure result
                   (Left err) ->
                     ioError $ userError $ "The rebac schema from the plugins' fragments was rejected, nothing was changed:\n" ++ show (ppReconcileError err)

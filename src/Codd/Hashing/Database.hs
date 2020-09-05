module Codd.Hashing.Database (getDbHashes) where

import Codd.Hashing.Types
import Control.Monad (forM)
import Data.Text (Text)
import Data.String (IsString(..))
import Database.PostgreSQL.Simple (ToRow, Query)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO, MonadIO(..))

hashProjection :: [QueryFrag] -> QueryFrag
hashProjection cols = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    toHash col = "(CASE WHEN " <> col <> " IS NULL THEN '' ELSE '_' || " <> col <> " :: TEXT END)"
    interspBy _ [] = ""
    interspBy _ (c:[]) = c
    interspBy sep (c:cs) = c <> sep <> interspBy sep cs

data QueryFrag = forall a. ToRow a => QueryFrag Query a
instance IsString QueryFrag where
    fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
    QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 DB.:. p2)

withQueryFrag :: QueryFrag -> (forall a. ToRow a => Query -> a -> b) -> b
withQueryFrag (QueryFrag q args) f = f q args

queryObjNamesAndHashes :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [QueryFrag] -> QueryFrag -> Maybe QueryFrag -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes conn objNameCol hashCols table filterBy = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection hashCols <> " FROM " <> table
                     <> maybe "" (" WHERE " <>) filterBy
                     <> " ORDER BY " <> objNameCol


getDbHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> m DbHashes
getDbHashes conn = do
    -- TODO: Do different installations of postgres come with different schemas? We should ask the User which schemas to consider
    schemas :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "schema_name" ["schema_owner", "default_character_set_catalog", "default_character_set_schema", "default_character_set_name" ] "information_schema.schemata" (Just $ QueryFrag "schema_name NOT IN ?" (DB.Only $ DB.In [ "information_schema" :: Text, "pg_catalog", "pg_temp_1", "pg_toast", "pg_toast_temp_1" ]))
    DbHashes <$> getSchemaHash conn schemas

getSchemaHash :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> [(ObjName, ObjHash)] -> m [SchemaHash]
getSchemaHash conn schemas = forM schemas $ \(schemaName, schemaHash) -> do
    -- TODO: do it all in a single query? it'd be nice to batch.. maybe if we could batch we could use haxl!!
    tables :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["table_type", "self_referencing_column_name", "reference_generation", "is_insertable_into", "is_typed", "commit_action"] "information_schema.tables" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    views :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["view_definition", "check_option", "is_updatable", "is_insertable_into", "is_trigger_updatable", "is_trigger_deletable", "is_trigger_insertable_into"] "information_schema.views" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    tableHashes <- getTablesHashes conn schemaName tables
    return $ SchemaHash schemaName schemaHash $ tableHashes ++ map (uncurry ViewHash) views

getTablesHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> ObjName -> [(ObjName, ObjHash)] -> m [SchemaObjectHash]
getTablesHashes conn schemaName tables = forM tables $ \(tableName, tableHash) -> do
    columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tableName))
    constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "constraint_name" ["constraint_type", "is_deferrable", "initially_deferred", "enforced"] "information_schema.table_constraints" (Just $ QueryFrag "constraint_schema=? AND table_name=?" (schemaName, tableName))
    pure $ TableHash tableName tableHash (map (uncurry TableColumn) columns) (map (uncurry TableConstraint) constraints)
{-# LANGUAGE CPP #-}

module Codd.Representations.Diff
    ( diffDbRep
    , filterDiff
    , combinePredicates
    , ignoreColumnOrderP
    , ignoreRoutineDefinitionMd5P
    , eqIgnoring
    , diffToDiffType
    ) where

import           Codd.Representations.Disk
import           Codd.Representations.Types

import           Data.Aeson                     ( Value(Object) )
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap             as KM
#else
import qualified Data.HashMap.Strict           as HM
#endif
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Merge.Strict         as MapMerge
import           Data.Text                      ( Text )

-- | Calculate 'DbRep' differences, based on equality
diffDbRep
    :: DbRep
    -> DbRep
    -> Map FilePath (ObjectRep, (Maybe Value, Maybe Value))
diffDbRep dbL dbR =
    MapMerge.merge
      (MapMerge.mapMissing . const $ fmap ((, Nothing) . Just))
      (MapMerge.mapMissing . const $ fmap ((,) Nothing . Just))
      (MapMerge.zipWithMaybeMatched . const $ go)
      (Map.fromList $ toFiles' dbL)
      (Map.fromList $ toFiles' dbR)
  where
    go
      :: (ObjectRep, Value)
      -> (ObjectRep, Value)
      -> Maybe (ObjectRep, (Maybe Value, Maybe Value))
    go l@(objectRepL, valueL) r@(_objectRepR, valueR)
      | l == r = Nothing
      | otherwise = Just (objectRepL, (Just valueL, Just valueR))

-- | Filter a difference map using the provided equality predicate
--
-- Note that the predicate should return 'True' when the values are
-- considered equal.  Entries are /removed/ in this case, which is the
-- opposite of normal filter semantics.
filterDiff
    :: (ObjectRep -> Value -> Value -> Bool)
    -- ^ Predicate that returns 'True' when values are considered equal
    -> Map FilePath (ObjectRep, (Maybe Value, Maybe Value))
    -> Map FilePath (ObjectRep, (Maybe Value, Maybe Value))
filterDiff p = Map.filter $ \case
    (objectRep, (Just valueL, Just valueR)) -> not $ p objectRep valueL valueR
    _otherwise -> True

-- | Combine equality predicates into a single equality predicate
combinePredicates
    :: [ObjectRep -> Value -> Value -> Bool]
    -> ObjectRep
    -> Value
    -> Value
    -> Bool
combinePredicates ps objectRep valueL valueR =
    or [p objectRep valueL valueR | p <- ps]

-- | Predicate for column equality ignoring column order
ignoreColumnOrderP :: ObjectRep -> Value -> Value -> Bool
ignoreColumnOrderP HColumn = eqIgnoring ["order"]
ignoreColumnOrderP _objectRep = const $ const False

-- | Predicate for routine equality ignoring definition MD5
ignoreRoutineDefinitionMd5P :: ObjectRep -> Value -> Value -> Bool
ignoreRoutineDefinitionMd5P HRoutine = eqIgnoring ["definition_md5"]
ignoreRoutineDefinitionMd5P _objectRep = const $ const False

-- | Equality of 'Value' ignoring the specified keys when the arguments
-- are of type 'Object'
--
-- This implementation is /not/ recursive.  The specified keys are ignored
-- only at the top level.
eqIgnoring :: [Text] -> Value -> Value -> Bool
eqIgnoring keys@(_:_) (Object mapL) (Object mapR) =
#if MIN_VERSION_aeson(2,0,0)
    let f kmap = foldr KM.delete kmap keys
    in  f mapL == f mapR
#else
    let f hmap = foldr HM.delete hmap keys
    in  f mapL == f mapR
#endif
eqIgnoring _keys valueL valueR = valueL == valueR

-- | Convert a difference map to a 'DiffType' map
diffToDiffType
    :: Map FilePath (ObjectRep, (Maybe Value, Maybe Value))
    -> Map FilePath DiffType
diffToDiffType = Map.mapMaybe diffType
  where
    diffType :: (ObjectRep, (Maybe Value, Maybe Value)) -> Maybe DiffType
    diffType (_objectRep, mValues) = case mValues of
      (Just dbValue, Just _expectedValue) -> Just $ BothButDifferent dbValue
      (Just dbValue, Nothing) -> Just $ NotExpectedButFound dbValue
      (Nothing, Just _expectedValue) -> Just ExpectedButNotFound
      (Nothing, Nothing) -> Nothing

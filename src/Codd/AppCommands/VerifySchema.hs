module Codd.AppCommands.VerifySchema
  ( verifySchema
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( withConnection )
import           Codd.Representations           ( detEncodeJSON
                                                , readRepresentationsFromDbWithSettings
                                                , readRepsFromDisk
                                                )
import           Codd.Representations.Diff      ( combinePredicates
                                                , diffDbRep
                                                , diffToDiffType
                                                , filterDiff
                                                , ignoreColumnOrderP
                                                , ignoreRoutineDefinitionMd5P
                                                )
import           Codd.Representations.Types     ( DbRep )
import           Control.Monad.Logger           ( MonadLoggerIO
                                                , logErrorN
                                                , logInfoN
                                                )
import           Data.Aeson                     ( decode )
import           Data.ByteString.Lazy           ( hGetContents )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Time                      ( secondsToDiffTime )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hSetBinaryMode )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                , stdin
                                                )

verifySchema
  :: (MonadUnliftIO m, MonadLoggerIO m)
  => CoddSettings
  -> Bool  -- ^ From @STDIN@?
  -> Bool  -- ^ Ignore column order?
  -> Bool  -- ^ Ignore function definitions?
  -> m ()
verifySchema dbInfoWithAllMigs@CoddSettings { onDiskReps, migsConnString } fromStdin ignoreColOrder ignoreFunDef
  = do
    let dbInfoDontApplyAnything = dbInfoWithAllMigs { sqlMigrations = [] }
    expectedSchemas :: DbRep <- if fromStdin
      then do
        liftIO $ hSetBinaryMode stdin True
        inputs <- liftIO $ hGetContents stdin
        pure
          $ fromMaybe
              (error
                "Could not decode the JSON input as a DB-schema representation. Make sure it is the output of 'codd write-schema --to-stdout' and that the versions of codd are exactly the same."
              )
          $ decode inputs
      else either readRepsFromDisk pure onDiskReps
    dbSchema <- withConnection
      migsConnString
      (secondsToDiffTime 5)
      (readRepresentationsFromDbWithSettings dbInfoDontApplyAnything)
    let diffFilter
          -- optimization to avoid linear traversal when not filtering
          | ignoreColOrder || ignoreFunDef = filterDiff $
              combinePredicates . map snd . filter fst $
                [ (ignoreColOrder, ignoreColumnOrderP)
                , (ignoreFunDef, ignoreRoutineDefinitionMd5P)
                ]
          | otherwise = id
        diffTypeMap
          = diffToDiffType
          . diffFilter
          $ diffDbRep dbSchema expectedSchemas
    if Map.null diffTypeMap
     then logInfoN "Database and expected schemas match."
     else do
        logErrorN $
          "DB and expected schemas do not match. Differing objects and their current DB schemas are: "
            <> detEncodeJSON diffTypeMap
        liftIO $ exitWith (ExitFailure 1)

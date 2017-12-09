{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(
runDB, doMigrations,
insertStartTime,insertEndTime,insertTotalStartTime,insertTotalEndTime,
insertFile,insertResult,
fetchTotal
) where

-- PERSISTENT
import Control.Monad.IO.Class
import Control.Monad.Logger    (runStderrLoggingT,runNoLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Pool

-- JSON
import qualified Data.Aeson.Parser
import Data.Aeson.Compat 
import Data.Aeson.Types
import Data.Attoparsec.ByteString hiding (count)
import Data.ByteString (ByteString)

-- TIME
import Data.Time.LocalTime
import Data.Time

-- READER
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)

-- MISC
import Data.Maybe (fromJust)

import Utils
import Config
import Models

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDB :: SqlPersistT IO b -> AppProcess b
runDB query = do
  pool <- asks poolConfig
  liftIO $ runSqlPool query pool

insertStartTime :: Key Repository -> String ->  AppProcess ()
insertStartTime id commit = do
  time <- liftIO getCurrentTime
  runDB $ insert $ CommitInfo id commit time Nothing  
  return ()

insertEndTime :: Key Repository -> String -> AppProcess ()
insertEndTime id commit = do
  time <- liftIO getCurrentTime
  runDB $ updateWhere [ CommitInfoRepositoryId ==. id
                      , CommitInfoCommit_str ==. commit ] 
                      [ CommitInfoEnd_time =. Just time ]
  return ()

insertFile :: Key Repository -> String -> String -> AppProcess ()
insertFile id commit file = do
  runDB $ insert $ CommitResults id commit file Nothing 
  return ()

insertResult :: Key Repository -> String -> String -> String -> AppProcess ()
insertResult id commit file res = do
  runDB $ updateWhere [ CommitResultsRepositoryId ==. id
              , CommitResultsFile_name ==. file ] 
              [CommitResultsComplexity =. Just res]
  return ()

--insertTotalStartTime :: String -> Int -> AppProcess (Key Repository)
insertTotalStartTime url nodes = do
  time <- liftIO getCurrentTime
  id <- insert $ Repository url nodes time Nothing
  return id

--insertTotalEndTime :: String -> Int -> AppProcess ()
insertTotalEndTime url nodes = do
  time <- liftIO getCurrentTime
  updateWhere [ RepositoryNodes ==. nodes
                      , RepositoryUrl ==. url] 
                      [ RepositoryEnd_time =. Just time]
  return ()

fetchTotal :: Key Repository -> String -> AppProcess Int
fetchTotal id commit = do
  runDB $ count [ CommitResultsRepositoryId ==. id
                , CommitResultsCommit_str ==. commit]


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
import Prelude hiding (id)

import Utils
import Config
import Models

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDB :: SqlPersistT IO b -> AppProcess b
runDB query = do
  pool <- asks poolConfig
  liftIO $ runSqlPool query pool

insertStartTime :: String ->  AppProcess ()
insertStartTime commit = do
  id' <- asks id
  time <- liftIO getCurrentTime
  runDB $ insert $ CommitInfo id' commit time Nothing  
  return ()

insertEndTime :: String -> AppProcess ()
insertEndTime commit = do
  id' <- asks id
  time <- liftIO getCurrentTime
  runDB $ updateWhere [ CommitInfoRepositoryId ==. id'
                      , CommitInfoCommit_str ==. commit ] 
                      [ CommitInfoEnd_time =. Just time ]
  return ()

insertFile :: String -> String -> AppProcess ()
insertFile commit file = do
  id' <- asks id
  runDB $ insert $ CommitResults id' commit file Nothing 
  return ()

insertResult :: String -> String -> String -> AppProcess ()
insertResult commit file res = do
  id' <- asks id
  runDB $ updateWhere [ CommitResultsRepositoryId ==. id'
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


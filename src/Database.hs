{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(
Run(..),
initDB,runDB,
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
import Control.Monad.Trans.Reader
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Repository json
    url String
    nodes Int
    start_time UTCTime
    end_time UTCTime Maybe
    deriving Show
CommitInfo json
    repositoryId RepositoryId
    commit_str String
    start_time UTCTime 
    end_time UTCTime Maybe
    deriving Show
CommitResults json
    repositoryId RepositoryId
    commit_str String
    file_name String
    complexity String Maybe
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

--runDB :: SqlPersistT IO b -> m b
runDB query = do
  pool <- asks poolConfig
  liftIO $ runSqlPool query pool


insertStartTime = do
  time <- liftIO getCurrentTime
  id <- asks idConfig
  commit <- asks commitConfig
  runDB $ insert $ CommitInfo id commit time Nothing  
  return ()

insertEndTime = do
  time <- liftIO getCurrentTime
  id <- asks idConfig
  commit <- asks commitConfig
  runDB $ updateWhere [ CommitInfoRepositoryId ==. id
                      , CommitInfoCommit_str ==. commit ] 
                      [ CommitInfoEnd_time =. Just time ]
  return ()

insertFile file = do
  id <- asks idConfig
  commit <- asks commitConfig
  runDB $ insert $ CommitResults id commit file Nothing 
  return ()

insertResult file res = do
  id <- asks idConfig
  commit <- asks commitConfig
  runDB $ updateWhere [ CommitResultsRepositoryId ==. id
              , CommitResultsFile_name ==. file ] 
              [CommitResultsComplexity =. Just res]
  return ()

insertTotalStartTime url nodes = do
  time <- liftIO getCurrentTime
  id <- runDB $ insert $ Repository url nodes time Nothing
  return id

insertTotalEndTime url nodes = do
  time <- liftIO getCurrentTime
  runDB $ updateWhere [ RepositoryNodes ==. nodes
                      , RepositoryUrl ==. url] 
                      [ RepositoryEnd_time =. Just time]
  return ()


fetchTotal = do
  id <- asks idConfig
  commit <- asks commitConfig
  runDB $ count [ CommitResultsRepositoryId ==. id
                , CommitResultsCommit_str ==. commit]


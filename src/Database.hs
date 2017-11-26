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

-- MISC
import Data.Maybe (fromJust)
-- TODO -> Create data type for storing the complexity 

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

data Run = Run {
  url :: String,
  dir :: FilePath,
  nodes :: Int,
  commit :: String,
  id :: Key Repository
}

connStr = "host=localhost dbname=argon_test user=root password=root port=5432" 

initDB :: IO ConnectionPool
initDB = do
  pool <- runNoLoggingT $ createPostgresqlPool connStr 10
  runDB pool doMigrations
  return pool

runDB 
  :: Control.Monad.IO.Class.MonadIO m => 
     Data.Pool.Pool SqlBackend -> SqlPersistT IO a -> m a       
runDB pool query = liftIO $ runSqlPool query pool

doMigrations = runMigration migrateAll

insertStartTime id commit = do
  time <- liftIO getCurrentTime
  insert $ CommitInfo id commit time Nothing  
  return ()

insertEndTime id commit = do
  time <- liftIO getCurrentTime
  updateWhere [ CommitInfoRepositoryId ==. id
              , CommitInfoCommit_str ==. commit ] 
              [ CommitInfoEnd_time =. Just time ]
  return ()

insertFile id commit file = do
  insert $ CommitResults id commit file Nothing 
  return ()

insertResult id commit file res = do
  updateWhere [ CommitResultsRepositoryId ==. id
              , CommitResultsFile_name ==. file ] 
              [CommitResultsComplexity =. Just res]
  return ()

insertTotalStartTime url nodes = do
  time <- liftIO getCurrentTime
  id <- insert $ Repository url nodes time Nothing
  return id

insertTotalEndTime url nodes = do
  time <- liftIO getCurrentTime
  updateWhere [ RepositoryNodes ==. nodes
              , RepositoryUrl ==. url] 
              [RepositoryEnd_time =. Just time]
  return ()


fetchTotal id commit = count [ CommitResultsRepositoryId ==. id
                             , CommitResultsCommit_str ==. commit]


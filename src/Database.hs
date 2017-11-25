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
initDB,runDB,
insertStartTime,insertEndTime,
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


-- TODO -> Create data type for storing the complexity 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CommitInfo json
    commit_str String
    start_time UTCTime 
    end_time UTCTime Maybe
    deriving Show
CommitResults json
    commit_str String
    file_name String
    complexity String Maybe
    deriving Show
|]

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

insertStartTime commit = do
  time <- liftIO getCurrentTime
  insert $ CommitInfo commit time Nothing  
  return ()

insertEndTime commit = do
  time <- liftIO getCurrentTime
  updateWhere [CommitInfoCommit_str ==. commit ] [CommitInfoEnd_time =. Just time]
  return ()

insertFile commit file = do
  insert $ CommitResults commit file Nothing 
  return ()

insertResult commit (file,res) = do
  updateWhere [CommitResultsFile_name ==. file] [CommitResultsComplexity =. Just res]
  return ()

fetchTotal commit = count [CommitResultsCommit_str ==. commit] 

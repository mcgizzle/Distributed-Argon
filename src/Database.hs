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

-- PERSISTENT
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

-- JSON
import qualified Data.Aeson.Parser
import Data.Aeson.Compat 
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)

-- TIME
import Data.Time.LocalTime
import Data.Time

-- TODO -> Create data type for storing the complexity 
type Complexity = String 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CommitInfo json
    commit String
    start_time UTCTime default=CURRENT_TIME
    end_time UTCTime Maybe
    deriving Show
CommitResults json
    commit String
    file_name String
    complexity Complexity
    deriving Show
|]

connStr = "host=localhost dbname=argon_test user=root password=root port=5432" 

data Response = InsertFail | InsertSuccess

initDB :: IO ()
initDB = undefined

insertCommit :: String -> IO ()
insertCommit commit = runDB query
  where 
    query = do
      time <- liftIO getCurrentTime
      insertUnique $ CommitInfo commit time Nothing  
      return ()

updateCommitEndTime :: String -> IO ()
updateCommitEndTime commit = runDB query
  where 
    query = do
      time <- liftIO getCurrentTime
      updateWhere [CommitInfoCommit ==. commit ] [CommitInfoEnd_time =. Just time]

insertResult :: Complexity -> IO Response
insertResult = undefined

runDB query = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    query
    return ()

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Config where

import Database.Persist.Sql (runSqlPool)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                    createPostgresqlPool)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT,MonadIO)

import Database.Persist.Sql (Key)
import Control.Distributed.Process
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Models

-- Reader
data AppConfig = AppConfig {
                              poolConfig :: ConnectionPool
                           }
type AppProcess = ReaderT AppConfig Process

--connStr :: ConnectionString
connStr = "host=localhost dbname=fs_dev user=root password=root port=5432"

initConfig :: IO AppConfig
initConfig = do
        p <- makePool 
        return AppConfig {
                poolConfig = p
                          }
makePool :: IO ConnectionPool
makePool = do
        conn <- getConnString
        runStdoutLoggingT (createPostgresqlPool conn 1)


getConnString :: IO ConnectionString
getConnString = return connStr



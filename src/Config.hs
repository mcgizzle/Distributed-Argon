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

import Prelude hiding (id)
import Database.Persist.Sql 
import Database.Persist.TH  
import Database.Persist.Postgresql 
import Control.Monad.Logger 
import Control.Monad.Reader
import Control.Monad.State

import Control.Distributed.Process
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Models

-- Reader
data AppConfig = AppConfig {
                              poolConfig :: ConnectionPool,
                              id         :: Key Repository,
                              files'     :: [FilePath]
                           }
type AppProcess = ReaderT AppConfig (StateT [ProcessId] Process)

--connStr :: ConnectionString
connStr = "host=localhost dbname=fs_dev user=root password=root port=5432"

initConfig :: Key Repository -> [FilePath] -> IO AppConfig
initConfig key files = do
        p <- makePool 
        return AppConfig {
                poolConfig = p,
                id         = key,
                files'      = files
                          }
makePool :: IO ConnectionPool
makePool = do
        conn <- getConnString
        runNoLoggingT (createPostgresqlPool conn 1)


getConnString :: IO ConnectionString
getConnString = return connStr



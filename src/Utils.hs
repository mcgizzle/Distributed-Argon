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

module Utils(
plog,log,
Command,
runArgon,
Repo,getCommits,fetchCommit
) where

import System.IO
import Control.Monad.State
import System.FilePath
import Control.Exception
import Prelude hiding (log)
import System.Process
import System.Directory

import System.IO.Temp
import Control.Monad.Reader
import Control.Distributed.Process

import Data.List.Split

import Database.Persist.Sql (runSqlPool)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                    createPostgresqlPool)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT,MonadIO)

-- Reader
data AppConfig = AppConfig {
                              poolConfig :: ConnectionPool,
                              urlConfig :: String,
                              commitConfig :: String,
                              idConfig :: Int              
                           }
type AppProcess = ReaderT AppConfig Process

--connStr :: ConnectionString
connStr = "host=localhost dbname=fs_dev user=root password=root port=5432"

initConfig :: String -> String -> Int -> IO AppConfig
initConfig commit url id = do
        p <- makePool 
        return AppConfig {
                poolConfig = p,
                urlConfig = url,
                commitConfig = commit,
                idConfig = id
                          }
makePool :: IO ConnectionPool
makePool = do
        conn <- getConnString
        runStdoutLoggingT (createPostgresqlPool conn 1)


getConnString :: IO ConnectionString
getConnString = return connStr

-- LOG
log :: String -> IO ()
log = liftIO . putStrLn 

plog :: String -> Process ()
plog msg = say $ "-->" ++ msg

-- COMMANDS
type Command = (String,String)

sendCommand :: Command -> IO String
sendCommand (cmd,arg) = do
  (_,Just hout,_,ph) <- createProcess (proc cmd $ words arg){ std_out = CreatePipe }
  hGetContents hout

getDir :: String -> String
getDir = last . splitOn "/" 

-- ARGON
runArgon :: String -> IO String
runArgon file = sendCommand ("stack","exec -- argon --json "++file)

-- GIT
type Repo = (String,String)

cloneRepo :: String -> IO ()
cloneRepo url = do
  exists <- doesDirectoryExist $ getDir url
  unless exists $ callProcess "git" ["clone",url]
  return ()

getCommits :: String -> IO [String]
getCommits url = do
  cloneRepo url
  commits <- sendCommand ("git","--git-dir "++ (getDir url) ++"/.git log --pretty=format:'%H'")
  return $ map strip $ words commits

fetchCommit :: (String,String) -> IO ()
fetchCommit (url,commit) = do
  cloneRepo url
  readCreateProcess ((proc "git" ["reset","--hard",commit]){ cwd = Just (getDir url)}) ""
  return ()

strip :: [a] -> [a]
strip []  = []
strip [x] = []
strip xs  = tail (init xs)



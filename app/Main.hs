module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                   (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                              (createTransport,defaultTCPParameters)
import System.Environment                                 (getArgs)
import System.Exit
import System.FilePath

import Database.Persist.Sql
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (log)

import Lib 
import Utils
import Database
import Config

  
--startManager :: String -> String -> String -> String -> IO ()
startManager url host port p = do
  pool <- makePool  
  commits <- getCommits url 
  backend <- initializeBackend host port rtable
  runSqlPool doMigrations pool
  files <- getRecursiveContents $ getDir url
  startMaster backend $ \workers -> do
    id <- liftIO $ runSqlPool (insertTotalStartTime url (length workers)) pool
    mapM_ (\ commit -> do
      config <- liftIO $ initConfig id files 
      runStateT (runReaderT (runAlg masterSlave commit url workers) config) []
      runStateT (runReaderT (getResults commit 0 (length files)) config) []
      ) commits

    terminateAllSlaves backend
    liftIO $ runSqlPool (insertTotalEndTime url (length workers)) pool
  clearRepo url
  liftIO $ putStrLn "\nResults have been stored in the database."
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend    
    ["manager", url, host, port, p]  -> do 
      putStrLn "Satrting Manager Node"
      case p of
        "master-slave"    -> startManager url host port masterSlave
        "work-stealing" -> startManager url host port workSteal 
    _ -> putStrLn "Bad parameters"

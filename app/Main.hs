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

import Prelude hiding (log)

import Lib 
import Utils
import Database

startManager :: String -> String -> String -> IO ()
startManager url host port = do
  pool <- initDB
  
  commits <- getCommits url "Chat-Server"
  
  backend <- initializeBackend host port rtable
  
  startMaster backend $ \workers -> do
    id <- runDB pool $ insertTotalStartTime url (length workers) 
    mapM_ (\ commit -> do
              let runData = Run url "Chat-Server" (length workers) commit id
              manager runData workers pool) commits
    runDB pool $ insertTotalEndTime url (length workers)
    terminateAllSlaves backend
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
    ["manager", url, host, port]  -> do 
      putStrLn "Satrting Manager Node"
      startManager url host port
    _ -> putStrLn "Bad parameters"

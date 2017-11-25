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
    mapM_ (\ commit -> manager (url,"Chat-Server",commit) workers pool) commits
    terminateAllSlaves backend
    liftIO $ putStrLn "Results have been stored in database."
  
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

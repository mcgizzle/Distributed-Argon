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

startManager :: [String] -> String -> String -> String -> IO ()
startManager commits url host port = do
  backend <- initializeBackend host port rtable
  startMaster backend $ \workers -> do
    result <- mapM (\ commit -> manager (url,"Chat-Server",commit) workers) commits
    terminateAllSlaves backend
    liftIO $ mapM_ (\ r -> putStrLn $ "RESULT:\n" ++ r) result
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
      commits <- getCommits url "Chat-Server"
      startManager commits url host port
    _ -> putStrLn "Bad parameters"

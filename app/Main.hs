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

startManager :: Files -> String -> IO ()
startManager files host = do
  backend <- initializeBackend host "5000" rtable
  startMaster backend $ \workers -> do
    result <- manager files workers
    terminateAllSlaves backend
    liftIO $ putStrLn $ "RESULT:\n" ++ result
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend    
    ["argon", path, host]  -> do 
      putStrLn "Satrting Manager Node"
      files <- getFiles path
      startManager files host
    _ -> putStrLn "Bad parameters"

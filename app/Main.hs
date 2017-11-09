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

port :: String
port = "600"

startWorkers :: Int -> String -> IO ()
startWorkers 0 _ = return ()
startWorkers n host = do
  putStrLn $ port ++ show n
  backend <- initializeBackend host (port ++ show n) rtable
  startSlave backend
  startWorkers (n -1) host
    
startManager :: Files -> String -> IO ()
startManager files host = do
  backend <- initializeBackend host "5000" rtable
  putStrLn "Backend intitialised"
  startMaster backend $ \workers -> do
    result <- manager files workers
    plog result
    plog " Terminating slaves..."
    terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs
  let files = getFiles
  case args of
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend    
    ["argon", path, host, n] -> do 
      putStrLn "Satrting Manager Node"
      files <- splitFiles path
      --startWorkers (read n :: Int) host
      startManager files host
    _ -> putStrLn "Bad parameters"

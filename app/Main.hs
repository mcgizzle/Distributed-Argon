module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                   (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                              (createTransport,defaultTCPParameters)
import System.Environment                                 (getArgs)
import System.Exit

import Prelude hiding (log)

import Lib 
import Utils

main :: IO ()
main = do
  args <- getArgs
  let files = getFiles
  case args of 
    ["manager", host, port] -> do
      log "Starting manager node..."
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        result <- manager files workers
        plog result
        plog " Terminating slaves..."
        terminateAllSlaves backend
    ["worker", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "Bad parameters"

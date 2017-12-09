{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

-- Cloud Haskell
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                     (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                                (createTransport,defaultTCPParameters)
import Prelude hiding (log)

-- STM
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Data
import Data.Binary
import Data.Either

-- Library
import Utils
import Database
import Config
import Models

-- Pipes
import Pipes
import Pipes.Safe (runSafeT)
import Pipes.Prelude as P hiding (show,length)
import GHC.Generics (Generic)

-- Reader
import Control.Monad.Reader 

import Argon hiding (defaultConfig)

type WorkQueue = ProcessId
type Master = ProcessId

doWork :: String -> IO String 
doWork = runArgon

worker :: (Master, WorkQueue) -> Process ()
worker (manager, workQueue) = do
  me <- getSelfPid
  plog " Ready to work! " 
  run me
  where
    run :: ProcessId -> Process ()
    run me = do
      send workQueue me
      receiveWait[match work, match end]
      where
        work f = do
          plog $ " Working on: " ++ show f
          result <- liftIO $ doWork f
          send manager (f,result)
          plog $ " Finished work on: " ++ show f ++ " :) "
          run me 
        end () = do
          plog " Terminating worker "
          return ()

remotable['worker] 

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

--manager :: Key Repository -> String -> String -> [NodeId] -> AppProcess ()
manager id commit url workers = do
  me <- lift getSelfPid
  liftIO $ fetchCommit (url,commit)
  lift $ plog "\n\n -----  WORKING ON NEXT COMMIT ------\n\n"
  files <- liftIO $ getRecursiveContents (getDir url)
  Control.Monad.Reader.mapM_ (insertFile id commit) files
  insertStartTime id commit
  workQueue <- lift $ spawnLocal $ do 
    forM_ files (\ f -> do pid <- expect; send pid f) 
    forever $ do
      pid <- expect
      send pid ()
  lift $ forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  getResults 0 (length files)
  insertEndTime id commit
  return ()
  where 
    getResults :: Int -> Int -> AppProcess ()
    getResults curCount total = do
      unless (curCount == total) $ do 
        (f,res) <- lift expect
        lift $ plog $ " Received: "++ f
        insertResult id commit f res
        getResults (curCount + 1) total
     

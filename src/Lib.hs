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
import Prelude hiding (log,id)

-- STM
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- MONADS
import Control.Monad.State
import Control.Concurrent.Lifted

-- DATA
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

-- Worker functions ---------------------------------------------------------------
doWork :: String -> IO String 
doWork = runArgon

slave :: Master -> Process ()
slave manager = run manager manager

worker :: (Master, WorkQueue) -> Process ()
worker (manager, workQueue) = run workQueue manager

run :: ProcessId -> ProcessId -> Process ()
run p1 manager = do
  me <- getSelfPid
  plog " Ready to work! " 
  send p1 me
  receiveWait[match work, match end]
  where
    work f = do
      plog $ " Working on: " ++ show f
      result <- liftIO $ doWork f
      send manager (f,result)
      plog $ " Finished work on: " ++ show f ++ " :) "
      run p1 manager 
    end () = do
      plog " Terminating worker "
      return ()

remotable['worker,'slave] 

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable
----------------------------------------------------------------------------------
-- Useful lift function for processes ---------------------------------------------
liftP :: (Monad m, Monad (t1 m), MonadTrans t, MonadTrans t1) => m a -> t (t1 m) a
liftP p = lift $ lift p

-- Alogrithim Manager ------------------------------------------------------------
runAlg p commit url workers = do
  liftIO $ fetchCommit (url,commit)
  liftP $ plog "\n\n -----  WORKING ON NEXT COMMIT ------\n\n"
  files <- asks files'
  cfg <- ask
  Control.Monad.Reader.mapM_ (insertFile commit) files
  insertStartTime commit
  p workers 
  insertEndTime commit
  return ()

-- Different algorithms for distributing the work -------------------------------- 
workSteal :: [NodeId] -> AppProcess ()
workSteal workers = do
  files <- asks files'
  me <- liftP getSelfPid
  workQueue <- liftP $ spawnLocal $ do 
    forM_ files (\ f -> do pid <- expect; send pid f) 
    forever $ do
      pid <- expect
      send pid ()
  liftP $ forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
-----------------------------------------------------------------------------------
masterSlave :: [NodeId] -> AppProcess ()
masterSlave workers = do
  me <- liftP getSelfPid
  files <- asks files'
  nids <- liftP $ forM workers $ \ nid -> spawn nid $ $(mkClosure 'slave) me
  pids <- liftP $ forM nids (\ _ -> expect) 
  lift $ put pids
  forM_ files (\ f -> do pid <- getPid; liftP $ send pid f)

getPid :: AppProcess ProcessId
getPid = do
  (p:pids) <- lift get
  lift $ put (pids ++ [p])
  return p
----------------------------------------------------------------------------------

getResults :: String -> Int -> Int -> AppProcess ()
getResults commit curCount total = do
  unless (curCount == total) $ do 
    (f,res) <- liftP expect
    liftP $ plog $ " Received: "++ f
    insertResult commit f res
    getResults commit (curCount + 1) total
     

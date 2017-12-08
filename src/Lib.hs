{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
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

-- Pipes
import Pipes
import Pipes.Safe (runSafeT)
import Pipes.Prelude as P hiding (show,length)
import GHC.Generics (Generic)

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

manager :: [NodeId] -> AppProcess String
manager workers = do
  me <- getSelfPid
  plog $ "\n\n ---  Fetching commit:  "++ commit  ++"------\n\n"
  count <- liftIO $ atomically $ newTVar 0 
  liftIO fetchCommit
  lftiIO insertStartTime
  let source = allFiles dir
  workQueue <- lift $ spawnLocal $ do 
    runSafeT $ runEffect $ for source $ lift . lift . dispatch
    total <- liftIO fetchTotal
    liftIO $ atomically $ writeTVar count total
    forever $ do
      pid <- expect
      send pid ()
  lift $ forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  getResults count 0
  liftIO insertEndTime
  return ()

--dispatch :: FilePath -> String -> Process ()
dispatch f = do 
  liftIO $ insertFile f
  pid <- expect
  send pid f


getResults :: TVar Int -> Int -> AppProcess ()
getResults count curCount = do
  count' <- liftIO $ atomically $ readTVar count
  unless (curCount == count') $ do 
    (f,res) <- expect :: Process (String,String)
    plog $ " Received: "++ f
    liftIO $ insertResult f res
    getResults count (curCount + 1) 
      

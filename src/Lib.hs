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

--manager :: Repo -> [NodeId] -> Int -> Process String
manager runData@Run{..} workers pool = do
  me <- getSelfPid
  plog $ "\n\n ---  Fetching commit:  "++ commit  ++"------\n\n"
  count <- liftIO $ atomically $ newTVar 0 
  liftIO $ fetchCommit (url,dir,commit)
  liftIO $ runDB pool $ insertStartTime id commit
  
  let source = allFiles dir
  workQueue <- spawnLocal $ do 
    runSafeT $ runEffect $ for source (\ f -> lift $ lift $ dispatch id f commit pool)
    total <- liftIO $ runDB pool $ fetchTotal id commit
    liftIO $ atomically $ writeTVar count total
    forever $ do
      pid <- expect
      send pid ()
  forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  getResults id count 0 commit pool
  
  liftIO $ runDB pool $ insertEndTime id commit
  return ()

--dispatch :: FilePath -> String -> Process ()
dispatch id f commit pool = do 
  liftIO $ runDB pool $ insertFile id commit f
  pid <- expect
  send pid f


--getResults :: TVar Int -> Int -> String -> IO ()
getResults id count curCount commit pool = do
  count' <- liftIO $ atomically $ readTVar count
  unless (curCount == count') $ do 
    (f,res) <- expect :: Process (String,String)
    plog $ " Received: "++ f
    liftIO $ runDB pool $ insertResult id commit f res
    getResults id count (curCount + 1) commit pool
      

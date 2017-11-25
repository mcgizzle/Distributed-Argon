{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
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
manager (url,dir,commit) workers pool = do
  me <- getSelfPid
  plog $ "\n\n ---  Fetching commit:  "++ commit  ++"------\n\n"
  count <- liftIO $ atomically $ newTVar (0) 
  liftIO $ fetchCommit (url,dir,commit)
  liftIO $ runDB pool $ insertStartTime commit
  
  let source = allFiles dir
  workQueue <- spawnLocal $ do 
    runSafeT $ runEffect $ for source (\ f -> lift $ lift $ dispatch f commit pool)
    total <- liftIO $ runDB pool $ fetchTotal commit
    liftIO $ atomically $ writeTVar count total
    forever $ do
      id <- expect
      send id ()
  forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  getResults count 0 commit pool
  return ()

--dispatch :: FilePath -> String -> Process ()
dispatch f commit pool = do 
  liftIO $ runDB pool $ insertFile commit f
  id <- expect
  send id f


--getResults :: TVar Int -> Int -> String -> IO ()
getResults count curCount commit pool = do
  count' <- liftIO $ atomically $ readTVar count
  unless (curCount == count') $ do 
    (f,res) <- expect :: Process (String,String)
    plog $ "Received: "++ f
    liftIO $ runDB pool $ insertResult commit (f,res)
    getResults count (curCount + 1) commit pool
      

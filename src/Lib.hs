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

-- Data
import Data.Binary
import Data.Either

-- Library
import Utils

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
          send manager result
          plog $ " Finished work on: " ++ show f ++ " :) "
          run me 
        end () = do
          plog " Terminating worker "
          send manager False
          return ()

remotable['worker] 

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

manager :: Repo -> [NodeId] -> Process String
manager (url,dir,commit) workers = do
  me <- getSelfPid
  plog $ "\n\n --------------  Fetching commit:  "++ commit  ++"---------------------\n\n"
  liftIO $ fetchCommit (url,dir,commit)
  let source = allFiles dir
  let dispatch f = do id <- expect; send id f
  workQueue <- spawnLocal $ do 
    runSafeT $ runEffect $ for source $ lift . lift . dispatch
    forever $ do
      id <- expect
      send id ()
  
  forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  getResult "" $ length workers

getResult :: String -> Int -> Process String
getResult s count = 
  receiveWait[match result, match done]
  where
    result r = getResult (s ++ r ++ "\n") count
    done False 
          | count == 1 = return s
          | otherwise = getResult s (count - 1)


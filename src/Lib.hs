{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where


import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                     (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                                (createTransport,defaultTCPParameters)
import Prelude hiding (log)

import Control.Monad.State

import Utils

import Pipes
import Pipes.Safe (runSafeT)
import Pipes.Prelude as P hiding (show,length)

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
          work <- liftIO $ doWork f
          send manager work
          plog " Finished work :) "
          run me 
        end () = do
          plog " Terminating worker "
          send manager False
          return ()

remotable['worker] 

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

manager :: FilePath -> [NodeId] -> Process String
manager path workers = do
  me <- getSelfPid
 
  let source = getFiles path
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
    result r = getResult (s ++ r) count
    done False 
          | count == 1 = return s
          | otherwise = getResult s (count - 1)


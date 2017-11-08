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

import System.FilePath


type File = String
type Files = [File]


type WorkQueue = ProcessId
type Master = ProcessId

log :: String -> Process ()
log msg = liftIO $ putStrLn msg 

doWork :: String -> String
doWork s = s ++ ": COMPLETE \n"

worker :: (Master, WorkQueue) -> Process ()
worker (manager, workQueue) = do
  me <- getSelfPid
  log $ "Worker started: " ++ show me
  run me
  where
    run :: ProcessId -> Process ()
    run me = do
      send workQueue me
      receiveWait
        [ match $ \n  -> do
            log $ "[Worker " ++ show me ++ "] given work: " ++ show n
            send manager $ doWork n
            log $ "[Worker " ++ show me ++ "] finished work."
            run me 
        , match $ \() -> do
            log $ "Terminating worker: " ++ show me
            return ()
        ]

remotable['worker] 

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

manager :: Files -> [NodeId] -> Process String
manager files workers = do
  me <- getSelfPid
  workQueue <- spawnLocal $ do 
    forM_ files $ \f -> do
      id <- expect
      send id f

    forever $ do
      log "Shutting down workers.."
      id <- expect
      send id ()
    
  forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (me,workQueue)
  log "Started workers"
 
  getResults $ length files

getResults :: Int -> Process File
getResults = run ""
  where
    run :: String -> Int -> Process String
    run r 0 = return r
    run r n = do
      s <- expect
      run (r ++ s ++ "\n") (n - 1)
      


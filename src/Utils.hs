{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Utils(
getDir,
plog,log,
getDir,getRecursiveContents,
Command,
runArgon,
Repo,getCommits,fetchCommit,
clearRepo
) where

import System.IO
import Control.Monad.State
import System.FilePath
import Control.Exception
import Prelude hiding (log)
import System.Process
import System.Directory
import Control.Monad
import System.IO.Temp
import Control.Distributed.Process
import Data.List.Split

-- MIC
getDir :: String -> String
getDir = last . splitOn "/" 

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (\f -> head f /= '.' && f /= "argon") names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory 
       then getRecursiveContents path 
       else return [path] 
  return (concat paths)

-- LOG
log :: String -> IO ()
log = liftIO . putStrLn 

plog :: String -> Process ()
plog msg = say $ "-->" ++ msg

-- COMMANDS
type Command = (String,String)

sendCommand :: Command -> IO String
sendCommand (cmd,arg) = do
  (_,Just hout,_,ph) <- createProcess (proc cmd $ words arg){ std_out = CreatePipe, std_err = Inherit }
  waitForProcess ph
  hGetContents hout

-- ARGON
runArgon :: String -> IO String
runArgon file = sendCommand ("stack","exec -- argon --json "++file)

-- GIT
type Repo = (String,String)

cloneRepo :: String -> IO ()
cloneRepo url = do
  exists <- doesDirectoryExist $ getDir url
  unless exists $ callProcess "git" ["clone",url]
  return ()

getCommits :: String -> IO [String]
getCommits url = do
  cloneRepo url
  commits <- sendCommand ("git","--git-dir "++ (getDir url) ++"/.git log --pretty=format:'%H'")
  return $ map strip $ words commits

fetchCommit :: (String,String) -> IO ()
fetchCommit (url,commit) = do
  cloneRepo url
  readCreateProcess ((proc "git" ["reset","--hard",commit]){ cwd = Just (getDir url)}) ""
  return ()

clearRepo :: String -> IO ()
clearRepo url = void $ callProcess "rm" ["-rf",getDir url]


strip :: [a] -> [a]
strip []  = []
strip [x] = []
strip xs  = tail (init xs)



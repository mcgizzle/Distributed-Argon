module Utils(
getDir,
plog,log,
Command,
runArgon,
Repo,getCommits,fetchCommit
) where

import System.IO
import Control.Monad.State
import System.FilePath
import Control.Distributed.Process hiding (try)
import Control.Exception
import Prelude hiding (log)
import System.Process
import System.Directory

import System.IO.Temp
import Data.List.Split
-- MIC
getDir :: String -> String
getDir = last . splitOn "/" 

-- LOG
log :: String -> IO ()
log = liftIO . putStrLn 

plog :: String -> Process ()
plog msg = say $ "-->" ++ msg

-- COMMANDS
type Command = (String,String)

sendCommand :: Command -> IO String
sendCommand (cmd,arg) = do
  (_,Just hout,_,ph) <- createProcess (proc cmd $ words arg){ std_out = CreatePipe }
  Process.waitForProcess ph
  hGetContents hout

-- ARGON
runArgon :: String -> IO String
runArgon file = sendCommand ("stack","exec -- argon --json "++file)

-- GIT
type Repo = (String,String,String)

cloneRepo :: String -> String -> IO ()
cloneRepo url dir = do
  exists <- doesDirectoryExist dir
  unless exists $ callProcess "git" ["clone",url]
  return ()

getCommits :: String -> String -> IO [String]
getCommits url dir = do
  cloneRepo url dir
  commits <- sendCommand ("git","--git-dir "++ dir ++"/.git log --pretty=format:'%H'")
  return $ map strip $ words commits

fetchCommit :: Repo -> IO ()
fetchCommit (url,dir,commit) = do
  cloneRepo url dir
  readCreateProcess ((proc "git" ["reset","--hard",commit]){ cwd = Just dir}) ""
  return ()

strip :: [a] -> [a]
strip []  = []
strip [x] = []
strip xs  = tail (init xs)



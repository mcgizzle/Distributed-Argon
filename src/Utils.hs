module Utils(
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

strip :: [a] -> [a]
strip []  = []
strip [x] = []
strip xs  = tail (init xs)

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
  hGetContents hout

-- UTIL COMMANDS
cd :: String -> IO ()
cd dir = callProcess "cd" [dir]

-- ARGON
runArgon :: String -> IO String
runArgon file = sendCommand ("stack","exec argon "++file)

-- GIT
type Repo = (String,String,String)

cloneRepo :: String -> String -> IO ()
cloneRepo url dir = do
  exists <- doesDirectoryExist dir
  unless exists $ callProcess "git" ["clone",url]
  cd dir
  putStrLn $ "Directory: " ++ dir 
  return ()

getCommits :: String -> String -> IO [String]
getCommits url dir = do
  cloneRepo url dir
  commits <- sendCommand ("git","--git-dir "++ dir ++".git log --pretty=format:'%H'")
  return $ map strip $ words commits

fetchCommit :: Repo -> IO ()
fetchCommit (url,dir,commit) = do
  cloneRepo url dir
  readCreateProcess ((proc "git" ["reset","--hard","--git-dir",dir ++".git",commit]){ cwd = Just dir}) ""
  return ()

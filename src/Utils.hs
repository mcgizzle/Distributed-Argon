module Utils(
plog,log,
Command,
runArgon
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

sendCommand_ :: Command -> IO ()
sendCommand_ (cmd,arg) = createProcess (proc cmd $ words arg) >> return ()
 
-- UTIL COMMANDS
cd :: String -> IO ()
cd dir = sendCommand_ ("cd",dir)

-- ARGON
runArgon :: String -> IO String
runArgon file = sendCommand ("stack","exec argon "++file)

-- GIT
cloneRepo :: String -> IO ()
cloneRepo repo = sendCommand_ ("git","clone "++repo)

getCommits :: String -> IO [String]
getCommits repo = do
  cd repo
  commits <- sendCommand ("git","log --pretty=format:'%H'")
  return $ lines commits

cloneCommit :: String -> IO ()
cloneCommit commit = cd commit >> sendCommand_ ("git","reset --hard "++commit)


module Utils(
File,Files,getFiles,splitFiles,
plog,log,
Command,
runArgon
) where

import System.IO
import Control.Monad.State
import System.FilePath
import Control.Distributed.Process
import Prelude hiding (log)
import System.Process
import System.Directory

-- FILES
type File = String
type Files = [File]

getFiles :: [String]
getFiles = ["WORK 1", "WORK 2", "WORK 3"]

-- LOG
log :: String -> IO ()
log = liftIO . putStrLn 

plog :: String -> Process ()
plog msg = say $ "-->" ++ msg

-- COMMANDS

type Command = (String,String)

splitFiles :: FilePath -> IO [FilePath]
splitFiles = listDirectory

runArgon :: String -> IO String
runArgon file = sendCommand ("cat", takeFileName file)

sendCommand :: Command -> IO String
sendCommand (cmd,arg) = do
  putStrLn "running cmd"
  (_,Just hout,_,_)  <- createProcess (proc cmd [arg]){ std_out = CreatePipe }
  hGetContents hout


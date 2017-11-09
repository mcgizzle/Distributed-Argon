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

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
    where f filename = filename /= "." && filename /= ".."


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
runArgon file = sendCommand ("stack","exec argon src/"++file)

sendCommand :: Command -> IO String
sendCommand (cmd,arg) = do
  putStrLn $ "running cmd: " ++ cmd ++ " " ++ arg
  (_,Just hout,_,_)  <- createProcess (proc cmd $ words arg){ std_out = CreatePipe }
  hGetContents hout


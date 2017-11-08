module Utils(
File,Files,getFiles,
plog,log
) where

import Control.Monad.State
import System.FilePath
import Control.Distributed.Process
import Prelude hiding (log)

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




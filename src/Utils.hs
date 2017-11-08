module Utils(getFiles) where
import Control.Monad.State

import System.FilePath

--newtype Files = State [FilePath]

getFiles :: [String]
getFiles = ["WORK 1", "WORK 2", "WORK 3"]

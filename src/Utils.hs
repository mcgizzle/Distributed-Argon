module Utils(getFiles) where
import Control.Monad.State

import System.FilePath

--newtype Files = State [FilePath]

getFiles :: [String]
getFiles = ["ME FILE", "HEY", "Print Me!"]

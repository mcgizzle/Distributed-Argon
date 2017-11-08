module Paths_RESTful_Cloud_API (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/McGroarty/Documents/College/Internet-Applications/RESTful-Cloud-API/.stack-work/install/x86_64-osx/ghc-7.10.2/7.10.2/bin"
libdir     = "/Users/McGroarty/Documents/College/Internet-Applications/RESTful-Cloud-API/.stack-work/install/x86_64-osx/ghc-7.10.2/7.10.2/lib/x86_64-osx-ghc-7.10.2/RESTful-Cloud-API-0.1.0.0-CEeMEH0yo3tGYfzjQEgYvg"
datadir    = "/Users/McGroarty/Documents/College/Internet-Applications/RESTful-Cloud-API/.stack-work/install/x86_64-osx/ghc-7.10.2/7.10.2/share/x86_64-osx-ghc-7.10.2/RESTful-Cloud-API-0.1.0.0"
libexecdir = "/Users/McGroarty/Documents/College/Internet-Applications/RESTful-Cloud-API/.stack-work/install/x86_64-osx/ghc-7.10.2/7.10.2/libexec"
sysconfdir = "/Users/McGroarty/Documents/College/Internet-Applications/RESTful-Cloud-API/.stack-work/install/x86_64-osx/ghc-7.10.2/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RESTful_Cloud_API_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RESTful_Cloud_API_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RESTful_Cloud_API_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RESTful_Cloud_API_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RESTful_Cloud_API_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

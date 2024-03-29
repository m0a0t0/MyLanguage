module Paths_myLanguage (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/matthew/.cabal/bin"
libdir     = "/home/matthew/.cabal/lib/myLanguage-0.2.0.1/ghc-7.4.1"
datadir    = "/home/matthew/.cabal/share/myLanguage-0.2.0.1"
libexecdir = "/home/matthew/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "myLanguage_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "myLanguage_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "myLanguage_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myLanguage_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

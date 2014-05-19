module Paths_haskell_token_utils (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alanz/.cabal/bin"
libdir     = "/home/alanz/.cabal/lib/x86_64-linux-ghc-7.6.3/haskell-token-utils-0.1.0.0"
datadir    = "/home/alanz/.cabal/share/x86_64-linux-ghc-7.6.3/haskell-token-utils-0.1.0.0"
libexecdir = "/home/alanz/.cabal/libexec"
sysconfdir = "/home/alanz/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_token_utils_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_token_utils_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_token_utils_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_token_utils_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_token_utils_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

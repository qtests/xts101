{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_xts101 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/bin"
libdir     = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/lib/x86_64-linux-ghc-8.4.3/xts101-0.1.0.0-1f4zAggTLxHBkvRxUCIXbZ-xts101"
dynlibdir  = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/share/x86_64-linux-ghc-8.4.3/xts101-0.1.0.0"
libexecdir = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/libexec/x86_64-linux-ghc-8.4.3/xts101-0.1.0.0"
sysconfdir = "/home/dokeris/hsk/xts101/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xts101_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xts101_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xts101_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xts101_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xts101_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xts101_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

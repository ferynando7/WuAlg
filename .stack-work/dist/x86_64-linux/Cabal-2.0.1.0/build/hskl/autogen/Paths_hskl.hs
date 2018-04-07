{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hskl (
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

bindir     = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/bin"
libdir     = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/lib/x86_64-linux-ghc-8.2.2/hskl-0.1.0.0-Jb7OHX0BTpw2cuhbL1sS68-hskl"
dynlibdir  = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/share/x86_64-linux-ghc-8.2.2/hskl-0.1.0.0"
libexecdir = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/libexec/x86_64-linux-ghc-8.2.2/hskl-0.1.0.0"
sysconfdir = "/home/ferz/VS Code/hskl/.stack-work/install/x86_64-linux/lts-10.8/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hskl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hskl_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hskl_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hskl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hskl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hskl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

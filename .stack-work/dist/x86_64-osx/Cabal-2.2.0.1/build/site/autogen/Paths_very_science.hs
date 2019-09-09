{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_very_science (
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

bindir     = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/bin"
libdir     = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3/very-science-0.1.0.0-HKorJvVufmuL4Aoi2jAMnN-site"
dynlibdir  = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/share/x86_64-osx-ghc-8.4.3/very-science-0.1.0.0"
libexecdir = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/libexec/x86_64-osx-ghc-8.4.3/very-science-0.1.0.0"
sysconfdir = "/Users/kwf/Sync/Web/very.science/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "very_science_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "very_science_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "very_science_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "very_science_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "very_science_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "very_science_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

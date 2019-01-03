{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_address (
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

bindir     = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/bin"
libdir     = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/lib/x86_64-osx-ghc-8.6.3/address-0.1.0.0-D7l6qYxXCYX6iDmiEMhxCX"
dynlibdir  = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/share/x86_64-osx-ghc-8.6.3/address-0.1.0.0"
libexecdir = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/libexec/x86_64-osx-ghc-8.6.3/address-0.1.0.0"
sysconfdir = "/Users/you/haskell/address/.stack-work/install/x86_64-osx/lts-13.1/8.6.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "address_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "address_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "address_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "address_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "address_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "address_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

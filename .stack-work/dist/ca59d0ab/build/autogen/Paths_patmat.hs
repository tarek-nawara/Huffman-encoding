{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_patmat (
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

bindir     = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\bin"
libdir     = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\lib\\x86_64-windows-ghc-8.0.2\\patmat-0.1.0.0-KLuXNrCZtHlLUAqfW2NWDz"
dynlibdir  = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\share\\x86_64-windows-ghc-8.0.2\\patmat-0.1.0.0"
libexecdir = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\libexec"
sysconfdir = "C:\\exercism\\haskell\\patmat\\.stack-work\\install\\58752683\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "patmat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "patmat_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "patmat_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "patmat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "patmat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "patmat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

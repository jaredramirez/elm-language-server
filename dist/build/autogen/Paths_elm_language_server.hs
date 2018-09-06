{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_elm_language_server (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jaredramirez/Library/Haskell/bin"
libdir     = "/Users/jaredramirez/Library/Haskell/ghc-8.2.2-x86_64/lib/elm-language-server-0.0.1"
dynlibdir  = "/Users/jaredramirez/Library/Haskell/ghc-8.2.2-x86_64/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/jaredramirez/Library/Haskell/share/ghc-8.2.2-x86_64/elm-language-server-0.0.1"
libexecdir = "/Users/jaredramirez/Library/Haskell/libexec"
sysconfdir = "/Users/jaredramirez/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "elm_language_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "elm_language_server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "elm_language_server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "elm_language_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "elm_language_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "elm_language_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

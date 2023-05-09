{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_vscode_haskell (
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

bindir     = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\vscode-haskell-0.1.0.0-inplace-vscode-haskell"
dynlibdir  = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2"
datadir    = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.10.2\\vscode-haskell-0.1.0.0"
libexecdir = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\vscode-haskell-0.1.0.0-inplace-vscode-haskell\\x86_64-windows-ghc-8.10.2\\vscode-haskell-0.1.0.0"
sysconfdir = "C:\\Users\\asi2m\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vscode_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vscode_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vscode_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vscode_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vscode_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vscode_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

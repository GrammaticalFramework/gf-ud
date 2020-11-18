{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_gf_ud (
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

bindir     = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/bin"
libdir     = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/lib/x86_64-linux-ghc-8.4.4/gf-ud-0.1.0.0-BzaOHV1u5cD80Paa8yhnZn-gf-ud"
dynlibdir  = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/share/x86_64-linux-ghc-8.4.4/gf-ud-0.1.0.0"
libexecdir = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/libexec/x86_64-linux-ghc-8.4.4/gf-ud-0.1.0.0"
sysconfdir = "/run/media/harisont/0684-7E46/GU/Thesis/gf-ud/.stack-work/install/x86_64-linux-tinfo6/561252f09fe50a8d08f8b780aa4a4c155679c412b551bf02f42883b489340f65/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gf_ud_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gf_ud_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gf_ud_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gf_ud_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gf_ud_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gf_ud_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

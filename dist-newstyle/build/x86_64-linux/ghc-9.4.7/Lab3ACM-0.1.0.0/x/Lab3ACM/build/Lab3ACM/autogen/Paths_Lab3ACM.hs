{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Lab3ACM (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/mark/.cabal/bin"
libdir     = "/home/mark/.cabal/lib/x86_64-linux-ghc-9.4.7/Lab3ACM-0.1.0.0-inplace-Lab3ACM"
dynlibdir  = "/home/mark/.cabal/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/mark/.cabal/share/x86_64-linux-ghc-9.4.7/Lab3ACM-0.1.0.0"
libexecdir = "/home/mark/.cabal/libexec/x86_64-linux-ghc-9.4.7/Lab3ACM-0.1.0.0"
sysconfdir = "/home/mark/.cabal/etc"

getBinDir     = catchIO (getEnv "Lab3ACM_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Lab3ACM_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Lab3ACM_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Lab3ACM_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab3ACM_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab3ACM_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
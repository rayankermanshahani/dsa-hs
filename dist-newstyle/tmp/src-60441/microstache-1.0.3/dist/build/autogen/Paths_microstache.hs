{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_microstache (
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
version = Version [1,0,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/rayker/.cabal/store/ghc-9.4.8/mcrstch-1.0.3-b29f8655/bin"
libdir     = "/Users/rayker/.cabal/store/ghc-9.4.8/mcrstch-1.0.3-b29f8655/lib"
dynlibdir  = "/Users/rayker/.cabal/store/ghc-9.4.8/lib"
datadir    = "/Users/rayker/.cabal/store/ghc-9.4.8/mcrstch-1.0.3-b29f8655/share"
libexecdir = "/Users/rayker/.cabal/store/ghc-9.4.8/mcrstch-1.0.3-b29f8655/libexec"
sysconfdir = "/Users/rayker/.cabal/store/ghc-9.4.8/mcrstch-1.0.3-b29f8655/etc"

getBinDir     = catchIO (getEnv "microstache_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "microstache_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "microstache_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "microstache_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "microstache_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "microstache_sysconfdir") (\_ -> return sysconfdir)



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

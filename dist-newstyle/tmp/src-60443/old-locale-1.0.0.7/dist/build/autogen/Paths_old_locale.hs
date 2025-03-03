{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_old_locale (
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
version = Version [1,0,0,7] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/rayker/.cabal/store/ghc-9.4.8/ld-lcl-1.0.0.7-59728c2f/bin"
libdir     = "/Users/rayker/.cabal/store/ghc-9.4.8/ld-lcl-1.0.0.7-59728c2f/lib"
dynlibdir  = "/Users/rayker/.cabal/store/ghc-9.4.8/lib"
datadir    = "/Users/rayker/.cabal/store/ghc-9.4.8/ld-lcl-1.0.0.7-59728c2f/share"
libexecdir = "/Users/rayker/.cabal/store/ghc-9.4.8/ld-lcl-1.0.0.7-59728c2f/libexec"
sysconfdir = "/Users/rayker/.cabal/store/ghc-9.4.8/ld-lcl-1.0.0.7-59728c2f/etc"

getBinDir     = catchIO (getEnv "old_locale_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "old_locale_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "old_locale_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "old_locale_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "old_locale_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "old_locale_sysconfdir") (\_ -> return sysconfdir)



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

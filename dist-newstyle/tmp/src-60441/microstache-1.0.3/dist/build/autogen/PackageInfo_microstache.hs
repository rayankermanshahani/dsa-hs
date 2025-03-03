{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_microstache (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "microstache"
version :: Version
version = Version [1,0,3] []

synopsis :: String
synopsis = "Mustache templates for Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskellari/microstache"

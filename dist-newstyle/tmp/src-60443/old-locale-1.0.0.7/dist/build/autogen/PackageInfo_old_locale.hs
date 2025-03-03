{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_old_locale (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "old_locale"
version :: Version
version = Version [1,0,0,7] []

synopsis :: String
synopsis = "locale library"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

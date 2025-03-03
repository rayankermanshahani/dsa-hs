{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_criterion_measurement (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "criterion_measurement"
version :: Version
version = Version [0,2,3,0] []

synopsis :: String
synopsis = "Criterion measurement functionality and associated types"
copyright :: String
copyright = "2009-2016 Bryan O'Sullivan and others"
homepage :: String
homepage = "https://github.com/haskell/criterion"

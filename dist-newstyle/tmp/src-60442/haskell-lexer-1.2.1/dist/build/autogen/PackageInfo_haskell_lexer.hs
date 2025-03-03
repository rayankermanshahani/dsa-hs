{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_haskell_lexer (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "haskell_lexer"
version :: Version
version = Version [1,2,1] []

synopsis :: String
synopsis = "A fully compliant Haskell 98 lexer"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/yav/haskell-lexer"

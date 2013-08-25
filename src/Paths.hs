{-# LANGUAGE CPP #-}
module Paths where

#ifdef CABAL
import qualified Paths_stunts_cartography
getDataDir = Paths_stunts_cartography.getDataDir
#else
getDataDir = return "." :: IO FilePath
#endif

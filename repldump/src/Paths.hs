{-# LANGUAGE CPP #-}
module Paths where

#ifdef CABAL
import Data.Version
import qualified Paths_stunts_cartography
#endif

#ifdef CABAL
versionString = Just $ showVersion Paths_stunts_cartography.version
#else
versionString = Nothing :: Maybe String
#endif

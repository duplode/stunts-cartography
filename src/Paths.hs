{-# LANGUAGE CPP #-}
module Paths where

#ifdef RESOURCES_WITH_EXECUTABLES
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
getDataDir = fmap takeDirectory getExecutablePath
#else
# ifdef CABAL
import qualified Paths_stunts_cartography
getDataDir = Paths_stunts_cartography.getDataDir
# else
getDataDir = return "." :: IO FilePath
# endif
#endif

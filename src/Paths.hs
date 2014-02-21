{-# LANGUAGE CPP #-}
module Paths where

#ifdef RESOURCES_WITH_EXECUTABLES
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
import qualified Paths_stunts_cartography
getDataDir = fmap takeDirectory getExecutablePath
#else
# ifdef CABAL
import qualified Paths_stunts_cartography
getDataDir = Paths_stunts_cartography.getDataDir
# else
getDataDir = return "." :: IO FilePath
# endif
#endif

#ifdef CABAL
versionString = Just $ Paths_stunts_cartography.version
#else
versionString = Nothing :: Maybe String
#endif

#ifdef RESOURCES_WITH_EXECUTABLES
isPortableBuild = True
#else
isPortableBuild = False
#endif

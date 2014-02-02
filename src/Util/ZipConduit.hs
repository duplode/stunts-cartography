{-# LANGUAGE PackageImports #-}
module Util.ZipConduit
    ( writeDirContentsZip
    ) where

import Control.Monad (filterM)
import Control.Applicative ((<$>))
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import "zip-conduit" Codec.Archive.Zip

writeDirContentsZip :: FilePath -> FilePath -> IO ()
writeDirContentsZip dir dest = do
    -- TODO: No existence check for dir.
    paths <- filterM doesFileExist
        =<< (map (dir </>)) <$> getDirectoryContents dir
    withArchive dest $ addFiles paths

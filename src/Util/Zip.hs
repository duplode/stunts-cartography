{-# LANGUAGE PackageImports #-}
module Util.Zip
    ( writeDirContentsZip
    ) where

import System.FilePath
import "zip" Codec.Archive.Zip

writeDirContentsZip :: FilePath -> FilePath -> IO ()
writeDirContentsZip dir dest = do
    -- TODO: No existence check for dir.
    let mkChangedEntrySel p =
            mkEntrySelector (takeBaseName dest </> makeRelative dir p)
    createArchive dest (packDirRecur Deflate mkChangedEntrySel dir)

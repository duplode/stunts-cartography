{-# LANGUAGE PackageImports #-}
module Util.Zip
    ( writeDirContentsZip
    ) where

import System.FilePath (takeBaseName)
import Path ((</>), stripDir, parseRelDir, parseAbsDir, parseAbsFile)
import "zip" Codec.Archive.Zip

writeDirContentsZip :: FilePath -> FilePath -> IO ()
writeDirContentsZip dir dest = do
    -- TODO: No existence check for dir.
    dir' <- parseAbsDir dir
    dest' <- parseAbsFile dest
    let mkChangedEntrySel p = do
            t <- parseRelDir (takeBaseName dest)
            p' <- stripDir dir' p
            mkEntrySelector (t </> p')
    createArchive dest' $
        packDirRecur Deflate mkChangedEntrySel dir'

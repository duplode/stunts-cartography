{-# LANGUAGE PackageImports #-}
module Util.ZipStreams
    ( writeDirContentsZip
    ) where

-- What follows was an attempt to use io-streams and zip-archive to make it
-- feasible to generate zip files with a lot of archives. While it works, it
-- zip-archive makes it spend too much memory. Eventually, we will want to
-- reimplement the relevant parts of zip-archive with io-stream; for now, this
-- module stays as a mere illustration.

import qualified System.IO.Streams as S
import qualified "zip-archive" Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as BL
import System.Directory
    (getDirectoryContents, getModificationTime, doesFileExist)
import System.FilePath (takeFileName, (</>))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad
import Control.Applicative ((<$>))

writeDirContentsZip :: FilePath -> FilePath -> IO ()
writeDirContentsZip dir dest = do
    fileStream <- S.fromList
        =<< map (dir </>) <$> getDirectoryContents dir
    archive <- S.foldM addLeafToArchive Z.emptyArchive fileStream
    BL.writeFile dest $ Z.fromArchive archive

addLeafToArchive :: Z.Archive -> FilePath -> IO Z.Archive
addLeafToArchive ar path = do
    exists <- doesFileExist path
    if exists
        then do
            modTime <- truncate . utcTimeToPOSIXSeconds
                <$> getModificationTime path
            mEntry <- S.withFileAsInput path $ \contentStream -> do
                mContent <- S.read contentStream
                return $ Z.toEntry (takeFileName path) modTime
                    . BL.fromStrict <$> mContent
            return $ maybe ar (`Z.addEntryToArchive` ar) mEntry
        else return ar


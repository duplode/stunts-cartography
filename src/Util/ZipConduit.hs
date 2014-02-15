{-# LANGUAGE PackageImports #-}
module Util.ZipConduit
    ( writeDirContentsZip
    ) where

import Control.Monad (filterM)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)
import "zip-conduit" Codec.Archive.Zip
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Lazy as BL

writeDirContentsZip :: FilePath -> FilePath -> IO ()
writeDirContentsZip dir dest = do
    -- TODO: No existence check for dir.
    paths <- filterM doesFileExist
        =<< (map (dir </>)) <$> getDirectoryContents dir
    withArchive dest $
        CL.sourceList paths $$ sinkFileWithChangedPath takeFileName

sinkFileWithChangedPath :: (FilePath -> FilePath) -> Sink FilePath Archive ()
sinkFileWithChangedPath fPath =
    CL.mapM (\p -> liftIO (BL.readFile p) >>= \bs -> return (fPath p, bs))
        =$ CL.mapM_ (\(p, bs) -> sinkEntry p $ CB.sourceLbs bs)

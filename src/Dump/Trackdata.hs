{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Dump.Trackdata
    ( Vec
    , VecWide
    , Trackdata (..)
    , parseFile
    ) where

import Dump.Common

import Control.Monad
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BS

data Trackdata = Trackdata
    { tdUnstructured1 :: BS.ByteString
    , trackdata09 :: [Vec]
    , trackdata10 :: [Vec]
    , tdUnstructured2 :: BS.ByteString
    } deriving Show

isZeroVec :: (Eq a, Num a) => (a, a, a) -> Bool
isZeroVec = \case
    (0, 0, 0) -> True
    _ -> False

getTrackdata :: B.Get Trackdata
getTrackdata = do
    tdUnstructured1 <- getUnstructured 0x177e
    trackdata09 <- takeWhile (not . isZeroVec) <$> replicateM 0x40 getVec16
    trackdata10 <- takeWhile (not . isZeroVec) <$> replicateM 0x30 getVec16
    tdUnstructured2 <- getUnstructured 0x51d5
    return Trackdata {..}

parseFile :: FilePath -> IO Trackdata
parseFile path = B.runGet getTrackdata <$> BS.readFile path

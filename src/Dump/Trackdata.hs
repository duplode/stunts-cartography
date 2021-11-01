{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Dump.Trackdata
    ( Vec
    , VecWide
    , Trackdata (..)
    , parseFile
    ) where

import Control.Monad
import Control.Arrow
import Control.Applicative
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BS
import Data.Int

type Vec = (Int, Int, Int)

type VecWide = (Integer, Integer, Integer)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

data Trackdata = Trackdata
    { tdUnstructured1 :: BS.ByteString
    , trackdata09 :: [Vec]
    , trackdata10 :: [Vec]
    , tdUnstructured2 :: BS.ByteString
    } deriving Show

getUint16 :: B.Get Int
getUint16 = liftM fi $ B.getWord16le

getUint32 :: B.Get Integer
getUint32 = liftM fi $ B.getWord32le

get3DVec :: B.Get a -> B.Get (a, a, a)
get3DVec coord = pure (,,) <*> coord <*> coord <*> coord

getVec16 :: B.Get Vec
getVec16 = get3DVec getUint16

getVec32 :: B.Get VecWide
getVec32 = get3DVec getUint32

getUnstructured :: Int -> B.Get BS.ByteString
getUnstructured len = liftM BS.pack . sequence $ replicate len B.getWord8

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

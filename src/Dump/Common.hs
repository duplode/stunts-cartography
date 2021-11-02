module Dump.Common
    ( Vec
    , VecWide
    , getUint8
    , getUint16
    , getUint32
    , getSint16
    , getSint32
    , getVec16
    , getVec32
    , getBool16
    , getUnstructured
    ) where

import Control.Monad
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BS

type Vec = (Int, Int, Int)

type VecWide = (Integer, Integer, Integer)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- We are not going to serialize this data back, so this lazy solution for
-- handling signed integers should suffice.
unTwos16Post :: Int -> Int
unTwos16Post x = if x < 2^15 then x else x - 2^16

unTwos32Post :: Integer -> Int
unTwos32Post x = if x < 2^31 then fi x else fi (x - 2^32)

getUint16 :: B.Get Int
getUint16 = liftM fi $ B.getWord16le

getUint32 :: B.Get Integer
getUint32 = liftM fi $ B.getWord32le

getSint16 :: B.Get Int
getSint16 = liftM (unTwos16Post . fi) $ B.getWord16le

getSint32 :: B.Get Int
getSint32 = liftM (unTwos32Post . fi) $ B.getWord32le

get3DVec :: B.Get a -> B.Get (a, a, a)
get3DVec coord = pure (,,) <*> coord <*> coord <*> coord

getVec16 :: B.Get Vec
getVec16 = get3DVec getUint16

getVec32 :: B.Get VecWide
getVec32 = get3DVec getUint32

getSVec16 :: B.Get Vec
getSVec16 = get3DVec getSint16

getSVec32 :: B.Get Vec
getSVec32 = get3DVec getSint32

getBool16 :: B.Get Bool
getBool16 = liftM ((/= 0) . fi) $ B.getWord16le

getUint8 :: B.Get Int
getUint8 = liftM fi $ B.getWord8

getUnstructured :: Int -> B.Get BS.ByteString
getUnstructured len = liftM BS.pack . sequence $ replicate len B.getWord8

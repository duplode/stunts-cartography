module Replay
    ( trkFromRplSimple
    , trackDataHasTheCorrectSize
    ) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Char (chr)
import qualified Util.ByteString as LB

-- Extracting tracks from replays. This will eventually be augmented with
-- proper .RPL processing tools. For now, this is an extremely crude solution.

data ReplayFormat = OneNil
                  | OneOne

-- Bare bones - no custom data type, no sanity checks. TODO: Convert this
-- into MaybeT / ErrorT to propagate failure.
trkFromRplSimple :: LB.ByteString -> (String, LB.ByteString)
trkFromRplSimple fileData = (trackName, trackData)
    where
    trackName = map (chr . fromIntegral) . LB.unpack
        . LB.takeWhile (/= 0) . LB.drop 0xD $ fileData
    trackData = rplTrackData fileData

detectRplFormat :: LB.ByteString -> ReplayFormat
detectRplFormat fileData
    | dataSize == declaredSizeOldFormat = OneNil
    | otherwise                         = OneOne
    -- Point of failure: Assuming that the data is in the new format just
    -- because the old format test failed obviously ignored that, for instance,
    -- there might not be enough bytes left in the data for a track.
    where
    dataSize = fromIntegral $ LB.length fileData
    -- Point of failure: a Nothing means there is no Word16 to read.
    declaredSizeOldFormat = fromMaybe 0 . fmap (0x722 +)
        . crudeTwoBytesLeToUInt . LB.drop 0x16 $ fileData

rplTrackData :: LB.ByteString -> LB.ByteString
rplTrackData fileData = LB.take 1802 . LB.drop trackOffset $ fileData
    where
    trackOffset = case detectRplFormat fileData of
        OneNil -> 0x18
        OneOne -> 0x1A

-- Basic validation. TODO: Consider integrating this into trkFromRplSimple .
trackDataHasTheCorrectSize :: LB.ByteString -> Bool
trackDataHasTheCorrectSize = (== 1802) . LB.length . rplTrackData

-- Just so that we don't have to import Data.Binary .
crudeTwoBytesLeToUInt :: LB.ByteString -> Maybe Int
crudeTwoBytesLeToUInt bs = do
    let twoBytes = LB.take 2 bs
    guard $ LB.length twoBytes == 2
    (little, big) <- fmap LB.head <$> LB.uncons twoBytes
    return $ fromIntegral little + 0x100 * fromIntegral big

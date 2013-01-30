module Utils
    ( splitAtEvery30th
    , bsSplitAtEvery30th
    ) where

import Data.List (groupBy, splitAt, unfoldr)
import qualified OurByteString as LB

splitAtEvery30th :: [a] -> [[a]]
splitAtEvery30th = splitAtIterated 30

splitAtIterated :: Int -> [a] -> [[a]]
splitAtIterated n = unfoldr $ \xs -> case xs of
    [] -> Nothing
    _  -> Just (splitAt n xs)

bsSplitAtEvery30th :: LB.ByteString -> [LB.ByteString]
bsSplitAtEvery30th = bsSplitAtIterated 30

bsSplitAtIterated :: Int -> LB.ByteString -> [LB.ByteString]
bsSplitAtIterated n = unfoldr $ \xs ->
    if LB.null xs
        then Nothing
        else Just $ LB.splitAt (fromIntegral n) xs

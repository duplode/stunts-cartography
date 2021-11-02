module Util.Misc
    ( bsChunksOf
    , trimFracPart
    ) where

import Data.List (unfoldr)
import qualified Data.ByteString.Lazy as LB

bsChunksOf :: Int -> LB.ByteString -> [LB.ByteString]
bsChunksOf n = unfoldr $ \xs ->
    if LB.null xs
        then Nothing
        else Just $ LB.splitAt (fromIntegral n) xs

trimFracPart :: RealFrac a => Int -> a -> a
trimFracPart n = (/ 10^n) . realToFrac . truncate . (* 10^n)

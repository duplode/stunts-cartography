module Util.Misc
    ( bsChunksOf
    ) where

import Data.List (splitAt, unfoldr)
import qualified Util.ByteString as LB

bsChunksOf :: Int -> LB.ByteString -> [LB.ByteString]
bsChunksOf n = unfoldr $ \xs ->
    if LB.null xs
        then Nothing
        else Just $ LB.splitAt (fromIntegral n) xs

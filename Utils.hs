module Utils
    ( splitAtEvery30th
    , bsSplitAtEvery30th
    , retrieveFileSize
    ) where

import Data.List (groupBy, splitAt, unfoldr)
import qualified OurByteString as LB
import Control.Exception (handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

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

--Lifted from RWH chapter 9.
retrieveFileSize :: FilePath -> IO (Maybe Integer)
retrieveFileSize path = handle nothingHandler $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)
    where
    nothingHandler :: IOError -> IO (Maybe Integer)
    nothingHandler = \_ -> return Nothing


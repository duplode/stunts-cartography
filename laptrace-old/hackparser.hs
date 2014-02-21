-- A sample parser for scanmem output generated as described in
-- instructions.txt .
module Main where

import Data.List (group)

-- Presumed format: 64-bit integers containing the z and x values.
-- It is made this way because scanmem can only watch 64 bits at one time.
-- We ignore the y coordinate, though I guess it would be possible to read it
-- from a second run and then match according to the z (or x) coordinates.
-- A more serious issue is that the x values are one step late wrt the x ones
-- as they are read from different vectors in the player state struct (likely
-- both vectors are used by the car motion algorithm). We will have to fix that
-- down the road.
readRawIntegers :: FilePath -> IO [Integer]
readRawIntegers path = readFile path >>= return . map read . lines

-- Suppresses repeated values in a list. Useful to eliminate bogus reads after
-- the coordinates are synced, or if the logging is done unconditionally. Note
-- that this is safe only if the car does not stop during the replay!
pruneRedundancies :: (Eq a) => [a] -> [a]
pruneRedundancies = map head . group

-- Splits the 64-bit integers obtained from scanmem into the two coordinates.
-- This implementations assumes the components were originally 32-bit little
-- endian integers, with z coming before x (see instructions.txt if that sounds
-- odd).
rawToCoords :: Integer -> (Integer, Integer)
rawToCoords raw = (twos rx, twos rz)
    where
    (rx, rz) = raw `quotRem` (2^32)
    twos n = if n < 2^63 then n else n - 2^64

-- Fix for the out-of-sync coordinates.
-- We assume that the coordinates in the very first step are correct, as x and
-- z (but not y!!) coordinates are equal in both vectors at the beginning of a
-- replay. We also drop the very last pair of coordinates, which should be fine
-- when dealing with finished laps.
-- Should we desire to add y coordinates to these pairs, we'd better pick them
-- from the late vector, dropping its first value.
syncCoordinates :: [(Integer, Integer)] -> [(Integer, Integer)]
syncCoordinates (first : rest) = first : zip (drop 1 xs) zs
    where
    (xs, zs) = unzip rest

formatOutput :: [(Integer, Integer)] -> String
formatOutput = unlines . map showCoords
    where
    showCoords (x, z) = show x ++ "\t" ++ show z

main = do
    raw <- readRawIntegers "vals2.dat"
    let coords = pruneRedundancies . syncCoordinates . map rawToCoords $ raw
    writeFile "out.dat" $ formatOutput coords

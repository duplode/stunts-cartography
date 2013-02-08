{-# LANGUAGE NoMonomorphismRestriction #-}
module LapTrace
    ( pathFromTrace
    , simpleRenderTracePath
    , readRawTrace
    ) where

import Diagrams.Prelude
import Palette (signCl)

scaleTrace :: [(Integer, Integer)] -> [(Double, Double)]
scaleTrace = map sclBoth
    where
    factor = 1/65536
    sclBoth (x, z) = (factor * fromIntegral x, factor * fromIntegral z)

dropLastSecond = reverse . drop 20 . reverse

dropFinal19Frames = reverse . drop 19 . reverse

pathFromTrace tr = tr
    # dropFinal19Frames # scaleTrace
    # map p2 # fromVertices

simpleRenderTracePath path = path
    # stroke
    # lw 0.05 # lc signCl
    -- # flip decoratePath (repeat $ square 0.1)

readRawTrace :: String -> [(Integer, Integer)]
readRawTrace dat =
    let nestedLists = filter (not . null)
            . map (map read . take 2 . words) . lines $ dat
        makePairs row = case row of
            (x : z : _) -> (x, z)
            _           -> error $ "Missing coordinates: "
                ++ "check the values " ++ show row ++ " in the input."
    in map makePairs nestedLists

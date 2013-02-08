{-# LANGUAGE NoMonomorphismRestriction #-}
module LapTrace
    ( pathFromTrace
    , simpleRenderTracePath
    , renderTracePathWithCars
    , readRawTrace
    ) where

import Diagrams.Prelude
import Pics (acura)
import Palette (signCl)

scaleTrace :: [(Integer, Integer)] -> [(Double, Double)]
scaleTrace = map sclBoth
    where
    factor = 1/65536
    sclBoth (x, z) = (factor * fromIntegral x, factor * fromIntegral z)

-- Ideally, we would drop 20 frames from a normal completed replay to get the
-- trace of the lap proper. Here, we discount one frame due to the sync
-- correction in the coordinate extraction process and another one to ensure
-- there will be a direction for every relevant frame when using cars to
-- decorate the path.
dropFinalFrames = reverse . drop 18 . reverse

pathFromTrace tr = tr
    # dropFinalFrames # scaleTrace
    # map p2 # fromVertices

simpleRenderTracePath path = path # stroke
    # lw 0.05 # lc signCl

renderTracePathWithCars path =
    let angles = (path :: Path (V P2))
            # explodePath # concat # drop 1
            # concatMap pathOffsets # map direction
            :: [Rad]
        cars = zipWith rotate angles
            (cycle $ raceCar : replicate 9 mempty)
    in decoratePath path cars

raceCar = acura signCl # scale 0.5

readRawTrace :: String -> [(Integer, Integer)]
readRawTrace dat =
    let nestedLists = filter (not . null)
            . map (map read . take 2 . words) . lines $ dat
        makePairs row = case row of
            (x : z : _) -> (x, z)
            _           -> error $ "Missing coordinates: "
                ++ "check the values " ++ show row ++ " in the input."
    in map makePairs nestedLists

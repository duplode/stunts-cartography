{-# LANGUAGE NoMonomorphismRestriction #-}
module LapTrace
    ( pathFromTrace
    , simpleRenderTracePath
    , renderTracePathWithCars
    , carsOnRails
    , animatedTrace
    , readRawTrace
    ) where

import Diagrams.Prelude
import Pics.MM (acura)
import Pics.Palette (signCl)

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

tracePoints tr = tr
    # dropFinalFrames # scaleTrace # map p2

pathFromTrace = fromVertices . tracePoints

simpleRenderTracePath path = path # stroke
    # lw 0.05 # lc signCl

pathAngles :: Path (V P2) -> [Rad]
pathAngles path = path
    # explodePath # concat # drop 1
    # concatMap pathOffsets # map direction

raceCar = acura signCl # scale 0.5

renderTracePathWithCars path =
    let cars = zipWith rotate
            (pathAngles path)
            (cycle $ raceCar : replicate 9 mempty)
    in decoratePath path cars

carsOnRails path = zipWith arrangeCar arrangement (repeat raceCar)
    where
    pathPos= path
        # pathVertices # concat
    arrangement = zip (pathPos) (pathAngles path)
    arrangeCar (pos, ang) = moveTo pos . rotate ang

animatedTrace path =
    let cars = carsOnRails path
    in  discrete cars

readRawTrace :: String -> [(Integer, Integer)]
readRawTrace dat =
    let nestedLists = filter (not . null)
            . map (map read . take 2 . words) . lines $ dat
        makePairs row = case row of
            (x : z : _) -> (x, z)
            _           -> error $ "Missing coordinates: "
                ++ "check the values " ++ show row ++ " in the input."
    in map makePairs nestedLists

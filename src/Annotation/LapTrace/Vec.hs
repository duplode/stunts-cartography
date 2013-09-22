module Annotation.LapTrace.Vec
    ( VecWide
    , VecDouble
    , scaleRawCoords
    , withXZ
    , scaleRawRot
    ) where

type VecWide = (Integer, Integer, Integer)

type VecDouble = (Double, Double, Double)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

scaleVecWide :: Double -> VecWide -> VecDouble
scaleVecWide q (x, y, z) = (q * fi x, q * fi y, q * fi z)

-- Scales coordinates from raw units to tiles.
scaleRawCoords :: VecWide -> VecDouble
scaleRawCoords = scaleVecWide (1 / 65536)

withXZ :: ((Double, Double) -> a) -> VecDouble -> a
withXZ f = f . (\(x,_,z) -> (x,z))

-- Scales a triple of angles from raw units to degrees.
scaleRawRot :: VecWide -> VecDouble
scaleRawRot = scaleVecWide (45 / 128)


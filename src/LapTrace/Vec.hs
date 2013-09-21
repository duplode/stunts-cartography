module LapTrace.Vec
    ( VecWide
    , VecDouble
    , scaleRawCoords
    , withXZ
    , vecToDegs
    ) where

import Data.Complex (polar, Complex(..))

type VecWide = (Integer, Integer, Integer)

type VecDouble = (Double, Double, Double)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- Scales coordinates from raw units to tiles.
scaleRawCoords :: VecWide -> VecDouble
scaleRawCoords (x, y, z) = (q * fi x, q * fi y, q * fi z)
    where
    q = 1 / 65536

withXZ :: ((Double, Double) -> a) -> VecDouble -> a
withXZ f = f . (\(x,_,z) -> (x,z))

-- Converts an orientation vector into two angles, the first one being the xz
-- angle and the other the angle in a plane perpendicular to it containing the
-- xz vector. Both angles are in degrees. The magnitude is discarded.
vecToDegs :: VecWide -> (Double, Double)
vecToDegs (x, y, z) = (toPositiveDeg xzPhs, toPositiveDeg aerialPhs)
    where
    (xzMag, xzPhs) = polar $ fi x  :+ fi z
    (_, aerialPhs) = polar $ xzMag :+ fi y
    toPositiveDeg  = (\d -> if d < 0 then d + 180 else d) . ((180 / pi) *)


{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track (Orientation, ElementType(..), TerrainType(..)
             , Tile(), getTileOrientation, getTerrainOrientation
             , getElementType, getTerrainType)

--rotateByOrient :: Orientation -> ("Dia" -> "Dia")
rotateByOrient = rotateBy . CircleFrac . (/4) . fromIntegral . fromEnum


--fitByElement, fitByTerrain :: "Dia" -> Tile -> "Dia"
fitByElement dia tile = dia # rotateByOrient (getTileOrientation tile)
fitByTerrain dia tile = dia # rotateByOrient (getTerrainOrientation tile)

--baseTerrainPic :: TerrainType -> "Dia"
baseTerrainPic tt = case tt of
    Plain -> genericSquare  green
    Water -> genericSquare  blue
    Hill -> genericSquare lightgreen
    AngledMargin ->
        rotateBy (-1/8) $
            squareTriangle # fc green # align (r2 (0, -1))
            ===
            reflectY (squareTriangle # fc blue)
    OuterAngledSlope ->
        baseTerrainPic Plain # alignBL
        # atop (squareTriangle
            # fc steelblue # rotateBy (3/8) # alignBL)
        -- # atop etc.
        # centerXY
    InnerAngledSlope ->
        baseTerrainPic Hill # alignBL
        # atop (squareTriangle
            # fc steelblue # rotateBy (-1/8) # alignBL)
        # centerXY
    _ -> genericSquare steelblue

--getTerrainPic :: Tile -> "Dia"
getTerrainPic tile =
    baseTerrainPic (getTerrainType tile)
    # rotateByOrient (getTerrainOrientation tile)

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0


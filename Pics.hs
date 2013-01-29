{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    , getTilePic
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
    Plain -> genericSquare plainCl
    Water -> genericSquare waterCl
    Hill -> genericSquare hillCl
    AngledMargin ->
        rotateBy (-1/8) $
            squareTriangle plainCl # align (r2 (0, -1))
            ===
            reflectY (squareTriangle waterCl)
    OuterAngledSlope ->
        baseTerrainPic Plain # alignBL
        # atop (squareTriangle slopeCl
            # rotateBy (3/8) # alignBL)
        -- # atop etc.
        # centerXY
    InnerAngledSlope ->
        baseTerrainPic Hill # alignBL
        # atop (squareTriangle slopeCl
            # rotateBy (-1/8) # alignBL)
        # centerXY
    Slope -> genericSquare slopeCl
    _ -> genericSquare lavaCl

--getTerrainPic :: Tile -> "Dia"
getTerrainPic tile =
    baseTerrainPic (getTerrainType tile)
    # rotateByOrient (getTerrainOrientation tile)

-- TODO: deal with materials.
--baseElementPic :: ElementType -> "Dia"
baseElementPic et = beneath emptySquare $ case et of
    Road ->
        hrule 1
        # lw 0.2 # fc tarmacCl
    _ -> mempty

--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    baseElementPic (getElementType tile)
    # rotateByOrient (getTileOrientation tile)

emptySquare = square 1 # lw 0

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle cl = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0 # fc cl

plainCl = green
waterCl = blue
hillCl = lightgreen
slopeCl = steelblue
lavaCl = coral

tarmacCl = darkgrey

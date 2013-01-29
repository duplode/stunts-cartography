{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    , getTilePic
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track (Orientation, Chirality
             , ElementType(..), ElementSurface(..), TerrainType(..)
             , Tile(), getTileOrientation, getTileChirality
             , getTileSurface, getTerrainOrientation
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
--baseElementPic :: Surface -> ElementType -> "Dia"
baseElementPic sf et = beneath emptySquare $ case et of
    Road ->
        hrule 1
        # lw 0.2 # lc (surfaceToColor sf)
    SharpCorner ->
        arc (0 :: CircleFrac) (1/4 :: CircleFrac) # moveOriginBy (r2 (1, 1))
        # scale 0.5 # lw 0.2 # lc (surfaceToColor sf)
    _ -> mempty

--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    baseElementPic (getTileSurface tile) (getElementType tile)
    # rotateByOrient (getTileOrientation tile)

emptySquare = square 1 # lw 0

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle cl = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0 # fc cl

surfaceToColor sf = case sf of
    Tarmac -> tarmacCl
    Dirt -> dirtCl
    Ice -> iceCl

plainCl = green
waterCl = blue
hillCl = lightgreen
slopeCl = steelblue
lavaCl = coral

tarmacCl = dimgrey
dirtCl = peru
iceCl = aliceblue

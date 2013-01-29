{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    , getTilePic
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track (Orientation(..), Chirality(..), rotateOrientation
             , ElementType(..), ElementSurface(..), TerrainType(..)
             , Tile(), getTileOrientation, getTileChirality, getTileSize
             , getTileSurface, getTerrainOrientation
             , getElementType, getTerrainType)

--rotateByOrient :: Orientation -> ("Dia" -> "Dia")
rotateByOrient = rotateBy . CircleFrac . (/4) . fromIntegral . fromEnum

--moveOriginBySize :: Orientation -> (Int, Int) -> ("Dia" -> "Dia")
moveOriginBySize q (lx, ly) =
    moveOriginBy $ r2 (delta q lx, delta (succ q) ly)
        where
        delta q z = (fromIntegral z - 1) * case q of
            Q1 -> -0.5
            Q2 -> 0.5
            Q3 -> 0.5
            Q4 -> -0.5

--reflectByChirality :: Chirality -> ("Dia" -> "Dia")
reflectByChirality c = case c of
    Sinistral -> reflectY
    _ -> id

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

--baseElementPic :: Surface -> ElementType -> "Dia"
baseElementPic sf et = case et of
    Road ->
        hrule 1
        # lw roadW # lc (surfaceToColor sf)
    SharpCorner ->
        arc (0 :: CircleFrac) (1/4 :: CircleFrac) # moveOriginBy (r2 (1, 1))
        # scale 0.5 # lw roadW # lc (surfaceToColor sf)
    LargeCorner ->
        baseElementPic sf SharpCorner
        # scale 3 # moveOriginBy (r2 (-0.5, -0.5))
    StartFinish ->
        baseElementPic sf Road
        # atop (eqTriangle (2 * roadW)
            # fc signCl # rotateBy (-1/4))
    SlalomRoad ->
        baseElementPic sf Road
        # atop (square (roadW * 3 / 4)
            # scaleX 0.5 # translate (r2 (-3/16, -roadW / 8)))
            # lw 0 # fc blockCl
        # atop (square (roadW * 3 / 4)
            # scaleX 0.5 # translate (r2 (3/16, roadW / 8)))
            # lw 0 # fc blockCl
    SharpSplit ->
        baseElementPic sf SharpCorner
            # atop (baseElementPic sf Road)
    LargeSplit ->
        baseElementPic sf LargeCorner
        # atop (baseElementPic sf Road
            # scale 2 # translateY 0.5)
    _ -> mempty

--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    baseElementPic (getTileSurface tile) (getElementType tile)
    # beneath (emptySquare
        # scaleX (fromIntegral . fst . getTileSize $ tile)
        # scaleY (fromIntegral . snd . getTileSize $ tile))
    # reflectByChirality (getTileChirality tile)
    # moveOriginBySize (getTileOrientation tile) (getTileSize tile)
    # rotateByOrient (getTileOrientation tile)

emptySquare = square 1 # lw markerW

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle cl = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0 # fc cl

markerW = 1 / 20
roadW = 1 / 5

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

signCl = yellow
blockCl = lightgrey

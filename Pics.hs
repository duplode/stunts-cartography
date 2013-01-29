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

orientationCorrection et q = case et of
    ElevatedCorner ->
        let (deltaX, deltaY) = (1 / (2 - 1/2)) * bridgeH * case q of
                Q1 -> (-1, 1)
                Q2 -> (1, 1)
                Q3 -> (1, -1)
                Q4 -> (-1, -1)
        in translateX deltaX . translateY deltaY
        . scaleX (1 + deltaX) . scaleY (1 + deltaY)
    _ -> id

--baseTerrainPic :: TerrainType -> "Dia"
baseTerrainPic tt = case tt of
    Plain ->
        genericSquare plainCl
    Water ->
        genericSquare waterCl
    Hill ->
        genericSquare hillCl
    AngledMargin ->
        rotateBy (-1/8) $
            squareTriangle plainCl # align unit_Y
            ===
            reflectY (squareTriangle waterCl)
    OuterAngledSlope ->
        baseTerrainPic Plain # alignBL
        # atop (squareTriangle slopeCl
            # rotateBy (3/8) # alignBL)
        # centerXY
    InnerAngledSlope ->
        baseTerrainPic Hill # alignBL
        # atop (squareTriangle slopeCl
            # rotateBy (-1/8) # alignBL)
        # centerXY
    Slope ->
        genericSquare slopeCl
    _ ->
        genericSquare lavaCl

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
        cornerArc (surfaceToColor sf) roadW 1
    LargeCorner ->
        cornerArc (surfaceToColor sf) roadW 2
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
    Tunnel ->
        genericSquare tunnelCl # scaleY (roadW * tunnelRelW)
    Crossroad ->
        baseElementPic sf Road
        # atop (baseElementPic sf Road
            # rotateBy (1/4))
    Highway ->
        baseElementPic sf Road # freeze # scaleY highwayRelW
        # atop (genericSquare hillCl # scaleY (hwDivideRelW * roadW))
    ElevatedSpan ->
        genericSquare bridgeCl # scaleY (roadW * bridgeRelW)
        # atop (baseElementPic sf Road)
        # translateY bridgeH
    SpanOverRoad ->
        baseElementPic sf Road # rotateBy (1/4)
        # atop (baseElementPic sf ElevatedSpan)
    ElevatedRoad ->
        cat' unitX with { sep = 2 * pillarW } (replicate 3 $
            genericSquare bridgeCl # scaleX pillarW
            # scaleY bridgeH # translateY (bridgeH / 2))
        # centerX
        # atop (baseElementPic sf ElevatedSpan)
    SolidRoad ->
        genericSquare bridgeCl
        # scaleY bridgeH # translateY (bridgeH / 2)
        # atop (baseElementPic sf ElevatedSpan)
    ElevatedCorner ->
        cornerArc bridgeCl (roadW * bridgeRelW) 2
        # atop (baseElementPic sf LargeCorner)
    _ -> mempty

--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    baseElementPic (getTileSurface tile) (getElementType tile)
    # orientationCorrection (getElementType tile) (getTileOrientation tile)
    # beneath (emptySquare
        # scaleX (fromIntegral . fst . getTileSize $ tile)
        # scaleY (fromIntegral . snd . getTileSize $ tile))
    # reflectByChirality (getTileChirality tile)
    # moveOriginBySize (getTileOrientation tile) (getTileSize tile)
    # rotateByOrient (getTileOrientation tile)
    -- Moving the origin as below seems to work if we don't care about where
    -- the `emptySquare`s of large elements end up.
    -- # moveOriginBySize Q1 (getTileSize tile)

emptySquare = square 1 # lw markerW

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle cl = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0 # fc cl

cornerArc cl w l =
    arc (0 :: CircleFrac) (1/4 :: CircleFrac)
    # alignBL # scale (l - 1/2) # moveOriginBy (r2 (l/2, l/2))
    # lw w # lc cl

markerW = 1 / 20
roadW = 1 / 5
tunnelRelW = 5 / 3
-- highwayRelW > 2
highwayRelW = 3
hwDivideRelW = highwayRelW - 2
bridgeRelW = 5 / 4
bridgeH = 1 / 4
pillarW = 1 / 10

surfaceToColor sf = case sf of
    Tarmac -> tarmacCl
    Dirt -> dirtCl
    Ice -> iceCl

plainCl = green
waterCl = blue
hillCl = lightgreen
slopeCl = steelblue
lavaCl = orangered

tarmacCl = dimgrey
dirtCl = peru
iceCl = aliceblue

signCl = yellow
tunnelCl = coral
blockCl = lightgrey
bridgeCl = blanchedalmond

{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    , getTilePic
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track (Orientation(..), Chirality(..), rotateOrientation
             , ElementType(..), ElementSurface(..), ElementAttribute(..)
             , TerrainType(..)
             , Tile(), getTileOrientation, getTileChirality, getTileSize
             , getTileSurface, getTerrainOrientation
             , getElementType, getTerrainType
             , isElemAttrOf )

--rotateByOrient :: Orientation -> ("Dia" -> "Dia")
rotateByOrient = rotateBy . CircleFrac . (/4) . fromIntegral . fromEnum

{-# INLINE corrSignumX #-}
corrSignumX q = case q of
    Q1 -> -1
    Q2 -> 1
    Q3 -> 1
    Q4 -> -1

{-# INLINE corrSignumY #-}
corrSignumY q = corrSignumX (succ q)

--moveOriginBySize :: Orientation -> (Int, Int) -> ("Dia" -> "Dia")
moveOriginBySize q (lx, ly) =
    let delta l = (fromIntegral l - 1) / 2
    in moveOriginBy $ r2 (corrSignumX q * delta lx, corrSignumY q * delta ly)

--reflectByChirality :: Chirality -> ("Dia" -> "Dia")
reflectByChirality c = case c of
    Sinistral -> reflectY
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
baseElementPic q sf et = case et of
    Road ->
        hrule 1
        # lw roadW # lc (surfaceToColor sf)
    SharpCorner ->
        cornerArc (surfaceToColor sf) roadW 1
    LargeCorner ->
        cornerArc (surfaceToColor sf) roadW 2
    StartFinish ->
        baseElementPic q sf Road
        # atop (eqTriangle (2 * roadW)
            # fc signCl # rotateBy (-1/4))
    SlalomRoad ->
        baseElementPic q sf Road
        # atop (square (roadW * 3 / 4)
            # scaleX 0.5 # translate (r2 (-3/16, -roadW / 8)))
            # lw 0 # fc blockCl
        # atop (square (roadW * 3 / 4)
            # scaleX 0.5 # translate (r2 (3/16, roadW / 8)))
            # lw 0 # fc blockCl
    SharpSplit ->
        baseElementPic q sf SharpCorner
            # atop (baseElementPic q sf Road)
    LargeSplit ->
        baseElementPic q sf LargeCorner
        # atop (baseElementPic q sf Road
            # scale 2 # translateY 0.5)
    Tunnel ->
        genericSquare tunnelCl # scaleY (roadW * tunnelRelW)
    Crossroad ->
        baseElementPic q sf Road
        # atop (baseElementPic q sf Road
            # rotateBy (1/4))
    Highway ->
        baseElementPic q sf Road # freeze # scaleY highwayRelW
        # atop (genericSquare hillCl # scaleY (hwDivideRelW * roadW))
    HighwayTransition ->
        isoscelesTransition tarmacCl highwayRelW
        # atop (eqTriangle (2 * sqrt 3 / 3) # alignT
            # scaleY (1/2) # scaleX (roadW * hwDivideRelW)
            # rotateBy (1/4) # lw 0 # fc hillCl)
    ElevatedSpan ->
        genericSquare bridgeCl # scaleY (roadW * bridgeRelW)
        # atop (baseElementPic q sf Road)
        # translateY bridgeH
    SpanOverRoad ->
        baseElementPic q sf Road # rotateBy (1/4)
        # atop (baseElementPic q sf ElevatedSpan)
    ElevatedRoad ->
        cat' unitX with { sep = 2 * pillarW } (replicate 3 $
            genericSquare bridgeCl
            # scaleX pillarW # scaleY bridgeH)
        # centerX
        # atop (baseElementPic q sf ElevatedSpan)
    SolidRoad ->
        genericSquare bridgeCl
        # scaleY bridgeH
        # atop (baseElementPic q sf ElevatedSpan)
    ElevatedCorner ->
        cornerArc bridgeCl (roadW * bridgeRelW) 2
        # atop (baseElementPic q sf LargeCorner)
        # elevatedCornerCorrection q
    ElevatedRamp ->
        rightTriangle bridgeCl bridgeH
        # clipBy (square 1 # translateX (-0.5))
        # rampBaseCorrection q
        # atop (rampTransition bridgeCl q sf)
    BridgeRamp ->
        rightTriangle fancyBridgeCl bridgeH
        # clipBy (square 1 # translateX (-0.5))
        # rampBaseCorrection q
        # atop (rampTransition fancyBridgeCl q sf)
    SolidRamp ->
        rightTriangle bridgeCl bridgeH
        # rampBaseCorrection q
        # atop (rampTransition bridgeCl q sf)
    BankedCorner ->
        let offset = (bankingH + roadW) / 2
        in cornerArc bankingCl bankingH 2
        # atop (cornerArc tarmacCl roadW (2 - offset)
            # translate (r2 (-offset / 2, -offset / 2)))
    _ -> mempty

elevatedCornerCorrection q =
    let correction = (1 / (2 - 1/2)) * bridgeH
        deltaX = corrSignumX q * correction
        deltaY = corrSignumY q * correction
    in translateX deltaX . translateY deltaY
    . scaleX (1 + deltaX) . scaleY (1 + deltaY)

rampCorrection q =
    shearY (corrSignumY q * bridgeH) `under` translationX 0.5

rampBaseCorrection q = scaleY (corrSignumY q)

emptySquare = square 1 # lw markerW

genericSquare cl = square 1 # lw 0 # fc cl

squareTriangle cl = polygon with
    { polyType = PolySides [1/4 :: CircleFrac] [1, 1]
    } # lw 0 # fc cl

cornerArc cl w l =
    arc (0 :: CircleFrac) (1/4 :: CircleFrac)
    # alignBL # scale (l - 1/2) # moveOriginBy (r2 (l/2, l/2))
    # lw w # lc cl

rightTriangle cl h =
    polygon with
        { polyType = PolySides [1/4 :: CircleFrac]
                               [ h, 1 ]
        , polyOrient = OrientV }
    # alignB # centerX # reflectY # translateY (h / 2)
    # lw 0 # fc cl

isoscelesTransition cl ratio =
    let sidePad = polygon with
            { polyType = PolySides [1/4 :: CircleFrac]
                                   [ roadW * (ratio - 1) / 2, 1 ]
            , polyOrient = OrientV }
            # lw 0 # fc cl # centerXY
    in centerY $
    sidePad # reflectY
    ===
    genericSquare cl # scaleY roadW
    ===
    sidePad

rampTransition cl q sf =
    isoscelesTransition cl bridgeRelW
    # atop (baseElementPic q sf Road # freeze)
    # rampCorrection q


--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    baseElementPic (getTileOrientation tile) (getTileSurface tile) (getElementType tile)
    -- # orientationCorrection (getElementType tile) (getTileOrientation tile)
    # beneath (emptySquare
        # scaleX (fromIntegral . fst . getTileSize $ tile)
        # scaleY (fromIntegral . snd . getTileSize $ tile))
    # reflectByChirality (getTileChirality tile)
    # moveOriginBySize (getTileOrientation tile) (getTileSize tile)
    # rotateByOrient (getTileOrientation tile)
    -- Moving the origin as below seems to work if we don't care about where
    -- the `emptySquare`s of large elements end up.
    -- # moveOriginBySize Q1 (getTileSize tile)

markerW = 1 / 20
roadW = 1 / 5
tunnelRelW = 5 / 3
-- highwayRelW > 2
highwayRelW = 3
hwDivideRelW = highwayRelW - 2
bridgeRelW = 5 / 4
bridgeH = 1 / 4
pillarW = 1 / 10
bankingH = 1 / 10

{-# INLINE surfaceToColor #-}
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
fancyBridgeCl = orchid
bankingCl = tomato

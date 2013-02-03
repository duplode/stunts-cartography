{-# LANGUAGE NoMonomorphismRestriction #-}
module Pics
    ( getTerrainPic
    , getTilePic
    , plainCl
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
        mempty
    Water ->
        genericSquare waterCl
    Hill ->
        genericSquare hillCl
    Slope ->
        genericSquare slopeCl
        # atop (vrule 0.5 # translateX 0.25
            # lw 0.1 # lc hillCl # lineCap LineCapRound)
    AngledMargin ->
        diagonalTriangle waterCl
    OuterAngledSlope ->
        diagonalTriangle slopeCl
    InnerAngledSlope ->
        diagonalTriangle hillCl
        # atop (diagonalTriangle slopeCl # rotateBy (1/2))
    _ ->
        genericSquare chasmCl

--getTerrainPic :: Tile -> "Dia"
getTerrainPic tile =
    baseTerrainPic (getTerrainType tile)
    # rotateByOrient (getTerrainOrientation tile)

baseElementPicNoC = baseElementPic Dextral

baseElementPicNoO = baseElementPicNoC Q1

-- Note that baseElementPic would work without recieving chiralities or
-- orientations weren't it for the "vertical" offset in the bridge graphics.

--baseElementPic :: Chirality -> Orientation -> Surface -> ElementType -> "Dia"
baseElementPic c q sf et = case et of
    Road ->
        genericSquare (surfaceToColor sf) # scaleY roadW
    SharpCorner ->
        cornerArc (surfaceToColor sf) roadW 1
    LargeCorner ->
        cornerArc (surfaceToColor sf) roadW 2
    StartFinish ->
        baseElementPicNoO sf Road
        # atop (eqTriangle (2 * roadW)
            # fc signCl # rotateBy (-1/4))
    SlalomRoad ->
        let slalomBlock = square (slalomRelW * roadW) # scaleX 0.5
                # translate (r2 (-3/16, -(1 - slalomRelW) * roadW / 2))
                # lw 0 # fc blockCl
        in baseElementPicNoO sf Road
        # atop slalomBlock
        # atop (slalomBlock # reflectX # reflectY)
    SharpSplit ->
        baseElementPicNoO sf SharpCorner
            # atop (baseElementPicNoO sf Road)
    LargeSplit ->
        baseElementPicNoO sf LargeCorner
        # atop (baseElementPicNoO sf Road
            # scaleX 2 # translateY 0.5)
    Tunnel ->
        genericSquare tunnelCl # scaleY (roadW * tunnelRelW)
    Crossroad ->
        baseElementPicNoO sf Road
        # atop (baseElementPicNoO sf Road
            # rotateBy (1/4))
    Highway ->
        baseElementPicNoO sf Road # scaleY highwayRelW
        # atop (genericSquare highwayCl # scaleY (hwDivideRelW * roadW))
    HighwayTransition ->
        isoscelesTransition tarmacCl highwayRelW
        # atop (baseElementPicNoO sf Road)
        # atop (eqTriangle (2 * sqrt 3 / 3) # alignT
            # scaleY (1/2) # scaleX (roadW * hwDivideRelW)
            # rotateBy (1/4) # lw 0 # fc highwayCl)
    ElevatedSpan ->
        genericSquare bridgeCl # scaleY (roadW * bridgeRelW)
        # atop (baseElementPicNoO sf Road)
        # translateY bridgeH
    SpanOverRoad ->
        baseElementPicNoO sf Road # rotateBy (1/4)
        # atop (baseElementPicNoO sf ElevatedSpan)
    ElevatedRoad ->
        cat' unitX with { sep = 2 * pillarW } (replicate 3 $
            genericSquare pillarCl
            # scaleX pillarW # scaleY bridgeH)
        # centerX # alignWithRoadY
        # atop (baseElementPicNoO sf ElevatedSpan)
    SolidRoad ->
        genericSquare pillarCl
        # scaleY bridgeH
        # alignWithRoadY
        # atop (baseElementPicNoO sf ElevatedSpan)
    ElevatedCorner ->
        cornerArc bridgeCl (roadW * bridgeRelW) 2
        # atop (baseElementPicNoO sf LargeCorner)
        # elevatedCornerCorrection q
    ElevatedRamp ->
        rightTriangle pillarCl bridgeH
        # clipBy (square 1 # translateX (-0.5))
        # alignWithRoadY
        # rampBaseCorrection q
        # atop (rampTransition bridgeCl q sf)
    BridgeRamp ->
        rightTriangle fancyPillarCl bridgeH
        # clipBy (square 1 # translateX (-0.5))
        # alignWithRoadY
        # rampBaseCorrection q
        # atop (rampTransition fancyBridgeCl q sf)
    SolidRamp ->
        rightTriangle pillarCl bridgeH
        # alignWithRoadY
        # rampBaseCorrection q
        # atop (rampTransition bridgeCl q sf)
    BankedCorner ->
        let outerLen = 2 + roadW * (1 - bankRelH) / 2
            innerLen = 2 - roadW * bankRelH
            offset = outerLen - innerLen
        in cornerArc bankCl (roadW * bankRelH) outerLen
        # atop (cornerArc tarmacCl roadW innerLen
            # translate (r2 (-offset / 2, -offset / 2)))
        # translate (r2 (outerLen / 2 - 1, outerLen / 2 - 1))
    BankedTransition ->
        rightTriangle bankCl (roadW * bankRelH)
        # centerY # translateY (-roadW * (1 - bankRelH) / 2)
        # atop (baseElementPicNoO sf Road
            # shearY (roadW * bankRelH) `under` translationX (0.5))
        # reflectY # rotateBy (-1/4)
    BankedRoad ->
        baseElementPicNoO sf Road # rotateBy (-1/4)
        # translateX (-roadW * bankRelH)
        |||
        genericSquare bankCl # scaleX (roadW * bankRelH)
    PipeTransition ->
        isoscelesTransition pipeCl pipeRelW
        # atop (baseElementPicNoO sf Road)
    Pipe ->
        genericSquare pipeCl # scaleY (roadW * pipeRelW)
        # atop (genericSquare meshCl # scaleY roadW)
    PipeObstacle ->
        baseElementPicNoO sf Pipe
        # atop (genericSquare pipeCl
            # scaleY roadW # scaleX (pipeObstacleRelW * roadW))
    CorkLeftRight ->
        baseElementPicNoO sf Road
        # atop (genericSquare meshCl
            # scaleY roadW # scaleX (1/2)
            # alignBL # shearX (1 / (2 * roadW)) # centerXY)
        # atop (fromSegments [ straight (r2 (1/2, -roadW)) ]
            # translate (r2 (-1/4, roadW / 2))
            # lw (corkWallRelW * roadW) # lc warningCl)
        # scaleX 2
    Loop ->
        let loopLeg = baseElementPicNoO sf Road
                # shearY (loopRelB * roadW) `under` translationX (0.5)
        in centerX (loopLeg ||| loopLeg # rotateBy (1/2))
        # atop (fromOffsets
            [ r2 (loopD, 0)
            , r2 (-2 * loopD, -2 * loopRelB * roadW)
            , r2 (loopD, 0)]
            # centerXY # lw roadW # lc meshCl)
    Chicane ->
        fromSegments [ bezier3 (r2 (1, 0)) (r2 (1, -1)) (r2 (2, -1)) ]
        # stroke # centerXY
        # lw roadW # lc (surfaceToColor sf)
    CorkUpDown ->
        baseElementPicNoO sf Road
        # translate (r2 (-0.5, 0.5))
        # atop (circle 0.5 # lw (bridgeRelW * roadW) # lc bridgeCl)
        # atop (circle 0.5 # lw roadW # lc tarmacCl)
        # atop (baseElementPicNoO sf ElevatedSpan
            # rampCorrection q
            # reflectByChirality c `under` translation (r2 (0.5, -bridgeH))
            # translate (r2 (0.5, 0.5 - bridgeH)))
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

alignWithRoadY = juxtapose unitY (hrule 1 # translateY (-roadW / 2))

emptySquare = square 1 # lw (1/20)

genericSquare cl = square 1 # lw 0 # fc cl

diagonalTriangle cl = polygon with
    { polyType = PolySides [-3/8 :: CircleFrac]
                           [ 1, sqrt 2 ]
    } # centerXY # lw 0 # fc cl

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
    strutY roadW
    ===
    sidePad

rampTransition cl q sf =
    isoscelesTransition cl bridgeRelW
    # atop (baseElementPicNoO sf Road)
    # rampCorrection q

--getTerrainPic :: Tile -> "Dia"
getTilePic tile =
    let c = getTileChirality tile
        q = getTileOrientation tile
        sf = getTileSurface tile
        et = getElementType tile
    in baseElementPic c q sf et
    # reflectByChirality c
    # moveOriginBySize q (getTileSize tile)
    # rotateByOrient q
    -- Moving the origin as below seems to work if we don't care about where
    -- the `emptySquare`s of large elements end up.
    -- # moveOriginBySize Q1 (getTileSize tile)

roadW = 1 / 5
slalomRelW = 2 / 3
pipeObstacleRelW = 1
corkWallRelW = 1 / 2
tunnelRelW = 5 / 3
hwDivideRelW = 1
highwayRelW = 2 + hwDivideRelW
bridgeRelW = 3 / 2
bridgeH = 1 / 4
pillarW = 1 / 10
bankRelH = 1 / 2
pipeRelW = 5 / 3
loopRelB = 1
loopD = 1 / 2

{-# INLINE surfaceToColor #-}
surfaceToColor sf = case sf of
    Tarmac -> tarmacCl
    Dirt -> dirtCl
    Ice -> iceCl

plainCl = forestgreen
waterCl = dodgerblue
hillCl = lightgreen
slopeCl = limegreen
chasmCl = midnightblue

tarmacCl = dimgrey
dirtCl = peru
iceCl = aliceblue

signCl = yellow
tunnelCl = coral
blockCl = lightgrey
highwayCl = lightgreen
bridgeCl = blanchedalmond
fancyBridgeCl = hotpink
fancyPillarCl = orchid
pillarCl = navajowhite
bankCl = tomato
pipeCl = darkgoldenrod
meshCl = darkgrey
warningCl = orangered

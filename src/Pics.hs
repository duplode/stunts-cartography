{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Pics
    ( getTerrainPic
    , getTilePic
    ) where

import Control.Monad (liftM)
import Control.Monad.Reader
import Diagrams.Prelude
import Diagrams.Coordinates ((&))
import Track (Orientation(..), Chirality(..), rotateOrientation
             , ElementType(..), ElementSurface(..), ElementAttribute(..)
             , TerrainType(..)
             , Tile(), getTileOrientation, getTileChirality, getTileSize
             , getTileSurface, getTerrainOrientation
             , getElementType, getTerrainType
             , isElemAttrOf )
import Palette
import qualified Parameters as Pm
import MM (acura)
import CartoM
import Types (BEDia)

--rotateByOrient :: Orientation -> ("Dia" -> "Dia")
rotateByOrient = rotateBy . Turn . (/4) . fromIntegral . fromEnum

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

baseTerrainPic :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m))
               => TerrainType -> QDiagram b R2 m
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

getTerrainPic :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m))
              => Tile -> QDiagram b R2 m
getTerrainPic tile =
    baseTerrainPic (getTerrainType tile)
    # rotateByOrient (getTerrainOrientation tile)


baseElementPicNoC env = baseElementPic' env Dextral

baseElementPicNoO env = baseElementPicNoC env Q1

-- Note that baseElementPic would work without recieving chiralities or
-- orientations weren't it for the "vertical" offset in the bridge graphics.
baseElementPic :: Chirality -> Orientation
               -> ElementSurface -> ElementType
               -> CartoM (Diagram BEDia R2)
baseElementPic c q sf et = do
    env <- ask
    return $ baseElementPic' env c q sf et

baseElementPic' env c q sf et = do
    let roadW = Pm.roadWidth env
        bridgeH = Pm.bridgeHeight env
        bridgeRelW = Pm.bridgeRelativeWidth env
        bankRelH = Pm.bankingRelativeHeight env
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

        cornerArc cl w l =
            arc (0 :: Turn) (1/4 :: Turn)
            # alignBL # scale (l - 1/2) # moveOriginBy (r2 (l/2, l/2))
            # lw w # lc cl

        isoscelesTransition cl ratio =
            let sidePad = polygon with
                    { polyType = PolySides [1/4 :: Turn]
                                           [ roadW * (ratio - 1) / 2, 1 ]
                    , polyOrient = OrientV }
                    # lw 0 # fc cl # centerXY
            in centerY $
            sidePad # reflectY
            ===
            strutY roadW
            ===
            sidePad

        rampTransition' flCl cl q =
            isoscelesTransition cl bridgeRelW
            # atop (genericSquare flCl # scaleY roadW)
            # rampCorrection q

        rampTransition = rampTransition' bridgeMeshCl

        rampTransitionSolid = rampTransition' tarmacCl

        spanSegment flCl =
            genericSquare bridgeCl # scaleY (roadW * bridgeRelW)
            # atop (genericSquare flCl # scaleY roadW)
            # translateY bridgeH

        sfMinW = 2 / 5
        sfMaxW = 4 / 5
        sfRelW = max (sfMinW / roadW) . min (sfMaxW / roadW) $ 2
        slalomRelW = 2 / 3
        pipeObstacleRelW = 1
        corkWallRelW = 1 / 2
        tunnelMaxW = 7 / 8
        tunnelRelW = min 2 (tunnelMaxW / roadW)
        highwayMaxW = 13 / 15
        hwDivideMaxW = 4 / 15
        hwDivideRelW = min (8 / 9) (hwDivideMaxW / roadW)
        highwayRelW = min (2 + hwDivideRelW) (highwayMaxW / roadW)
        pillarW = 1 / 10
        pipeRelW = 5 / 3
        loopMaxW = 7 / 8
        loopRelW = 21 / 8

        in case et of
            Road ->
                genericSquare (surfaceToColor sf) # scaleY roadW
            SharpCorner ->
                cornerArc (surfaceToColor sf) roadW 1
            LargeCorner ->
                cornerArc (surfaceToColor sf) roadW 2
            StartFinish ->
                baseElementPicNoO env sf Road
                # atop (eqTriangle (sfRelW * roadW)
                    # fc signCl # rotateBy (-1/4))
            SlalomRoad ->
                let slalomBlock = square (slalomRelW * roadW) # scaleX 0.5
                        # translate (r2 (-3/16, -(1 - slalomRelW) * roadW / 2))
                        # lw 0 # fc blockCl
                in baseElementPicNoO env sf Road
                # atop slalomBlock
                # atop (slalomBlock # reflectX # reflectY)
            SharpSplit ->
                baseElementPicNoO env sf SharpCorner
                    # atop (baseElementPicNoO env sf Road)
            LargeSplit ->
                baseElementPicNoO env sf LargeCorner
                # atop (baseElementPicNoO env sf Road
                    # scaleX 2 # translateY 0.5)
            Tunnel ->
                genericSquare tunnelCl # scaleY (roadW * tunnelRelW)
                # atop (genericSquare insideTunnelCl # scaleY roadW)
            Crossroad ->
                baseElementPicNoO env sf Road
                # atop (baseElementPicNoO env sf Road
                    # rotateBy (1/4))
            Highway ->
                baseElementPicNoO env sf Road # scaleY highwayRelW
                # atop (genericSquare highwayCl # scaleY (hwDivideRelW * roadW))
            HighwayTransition ->
                isoscelesTransition tarmacCl highwayRelW
                # atop (baseElementPicNoO env sf Road)
                # atop (eqTriangle (2 * sqrt 3 / 3) # alignT
                    # scaleY (1/2) # scaleX (roadW * hwDivideRelW)
                    # rotateBy (1/4) # lw 0 # fc highwayCl)
            ElevatedSpan ->
                spanSegment bridgeMeshCl # opacity 0.75
            SpanOverRoad ->
                baseElementPicNoO env sf Road # rotateBy (1/4)
                # atop (baseElementPicNoO env sf ElevatedSpan)
            ElevatedRoad ->
                cat' unitX with { sep = 2 * pillarW } (replicate 3 $
                    genericSquare pillarCl
                    # scaleX pillarW # scaleY bridgeH)
                # centerX # alignWithRoadY
                # atop (spanSegment bridgeMeshCl)
            SolidRoad ->
                genericSquare pillarCl
                # scaleY bridgeH
                # alignWithRoadY
                # atop (spanSegment tarmacCl)
            ElevatedCorner ->
                cornerArc bridgeCl (roadW * bridgeRelW) 2
                # atop (cornerArc bridgeMeshCl roadW 2)
                # elevatedCornerCorrection q
            ElevatedRamp ->
                rightTriangle pillarCl bridgeH
                # clipBy (square 1 # translateX (-0.5))
                # alignWithRoadY
                # rampBaseCorrection q
                # atop (rampTransition bridgeCl q)
            BridgeRamp ->
                rightTriangle fancyPillarCl bridgeH
                # clipBy (square 1 # translateX (-0.5))
                # alignWithRoadY
                # rampBaseCorrection q
                # atop (rampTransition fancyBridgeCl q)
            SolidRamp ->
                rightTriangle pillarCl bridgeH
                # alignWithRoadY
                # rampBaseCorrection q
                # atop (rampTransitionSolid bridgeCl q)
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
                # atop (baseElementPicNoO env sf Road
                    # shearY (roadW * bankRelH) `under` translationX (0.5))
                # reflectY # rotateBy (-1/4)
            BankedRoad ->
                baseElementPicNoO env sf Road # rotateBy (-1/4)
                # translateX (-roadW * bankRelH)
                |||
                genericSquare bankCl # scaleX (roadW * bankRelH)
            PipeTransition ->
                isoscelesTransition pipeCl pipeRelW
                # atop (baseElementPicNoO env sf Road)
            Pipe ->
                genericSquare pipeCl # scaleY (roadW * pipeRelW)
                # atop (genericSquare meshCl # scaleY roadW)
            PipeObstacle ->
                baseElementPicNoO env sf Pipe
                # atop (genericSquare pipeCl
                    # scaleY roadW # scaleX (pipeObstacleRelW * roadW))
            CorkLeftRight ->
                baseElementPicNoO env sf Road
                # atop (genericSquare meshCl
                    # scaleY roadW # scaleX (1/2)
                    # alignBL # shearX (1 / (2 * roadW)) # centerXY)
                # atop (fromSegments [ straight (r2 (1/2, -roadW)) ]
                    # translate (r2 (-1/4, roadW / 2))
                    # lw (corkWallRelW * roadW) # lc warningCl)
                # scaleX 2
            Looping ->
                let loopW = min (loopRelW * roadW) loopMaxW
                    loopB = (loopW - roadW) / 2
                    loopD = loopB / (2 * roadW)
                    loopLeg = baseElementPicNoO env sf Road
                        # shearY loopB `under` translationX (0.5)
                in centerX (loopLeg ||| loopLeg # rotateBy (1/2))
                # atop (fromOffsets
                    [ r2 (loopD, 0)
                    , r2 (-2 * loopD, -2 * loopB)
                    , r2 (loopD, 0)]
                    # centerXY # lw roadW # lc meshCl)
            Chicane ->
                fromSegments [ bezier3 (r2 (1, 0)) (r2 (1, -1)) (r2 (2, -1)) ]
                # stroke # centerXY
                # lw roadW # lc (surfaceToColor sf)
            CorkUpDown ->
                baseElementPicNoO env sf Road
                # translate (r2 (-0.5, 0.5))
                # atop (circle 0.5 # lw (bridgeRelW * roadW) # lc bridgeCl)
                # atop (circle 0.5 # lw roadW # lc tarmacCl)
                # atop (spanSegment tarmacCl
                    # rampCorrection q
                    # reflectByChirality c `under` translation (r2 (0.5, -bridgeH))
                    # translate (r2 (0.5, 0.5 - bridgeH)))
            Pine ->
                eqTriangle (5/8) # lc darkleafCl # fc leafCl
                ===
                square (1/8) # fc woodCl
            Palm ->
                let leaf = arc (1/8 :: Turn) (3/8 :: Turn)
                        # closeLine # strokeLoop
                        # scale 0.25 # lc darkleafCl # fc leafCl
                in beside unitY
                    (square (2/3) # scaleX (1/8) # fc woodCl)
                    ((leaf ||| leaf # reflectX) # centerX)
            Cactus ->
                (
                    fromOffsets [ 0 & (-3/16)
                                , (3/8) & 0
                                , 0 & (1/4)
                                ]
                    # stroke # centerXY
                    # lineJoin LineJoinRound
                    <> vrule (1/2))
                # lw 0.15 # lc cactusCl # lineCap LineCapRound
            TennisCourt ->
                (
                    hrule (3/4) # lw 0.1 # lc netCl
                    <> square (3/4) # fc tennisCl)
                # scaleX 0.5
            Ship ->
                (
                    polygon with
                        { polyType = PolySides [-1/4 :: Turn, -1/8 :: Turn]
                                               [ 1/4, 2/4, sqrt 2 * 1 / 4 ]
                        } # lw 0 # fc shipCl
                    ===
                    (vrule (1/2) # lc miscLightCl # alignT
                        ||| strutX (1/10) ||| square (1/5) # alignT
                        # lw 0 # fc miscDarkCl)
                    # centerX)
                # centerXY
            Barn ->
                let roofArc = wedge (1/2) (1/4 :: Turn) (5/12 :: Turn)
                        # closeLine # strokeLoop
                        # centerY
                    diagLine = hrule (1/3) # rotateBy (1/8)
                in (
                    diagLine <> reflectY diagLine
                    <> arc' (1/6) (0 :: Turn) (1 :: Turn))
                # translateX (1/20) # lw 0.025 # lc miscLightCl
                <> (
                    (reflectY roofArc <> roofArc) # scaleX 0.4
                    # lc barnCl # fc barnCl
                    ||| rect (2/5) (1/2) # lc barnCl # fc barnCl)
                # centerXY
            OfficeBuilding ->
                (
                    (
                        decoratePath (rect (1/4) (1/4))
                            (replicate 4 $ square (3/16) # lw 0  # fc miscDarkCl)
                        ||| strutX (1/16) ||| rect (1/8) (1/4) # lw 0 # fc miscDarkCl)
                    # alignR
                    <> rect (11/16) (1/2) # alignR # lw 0 # fc officeCl)
                # centerXY
            Windmill ->
                let blades = hrule (1/2) # lw 0.05 # rotateBy (1/8)
                        # lw 0 # lc windmillDetailsCl
                in (
                    reflectY blades <> blades
                    <> eqTriangle (2/5) # rotateBy (1/4) # centerXY
                    # lw 0 # fc windmillCl)
                ||| rect (3/20) (2/5) # fc windmillCl
                # alignL # lw 0
            GasStation ->
                (
                    vrule (3/10) # alignT
                    # lw 0.1 # lc gasRoofCl # lineCap LineCapRound
                    <> (
                        (
                            rect (3/16) (1/6) # alignR
                            # lw 0 # fc blueWindowCl
                            ===
                            strutY (1/16)
                            ===
                            rect (1/10) (3/32) # alignR
                            # lw 0 # fc ethanolCl
                            ===
                            strutY (1/16)
                            ===
                            rect (1/10) (3/32) # alignR
                            # lw 0 # fc petrolCl)
                        # centerY
                        <> roundedRect' (3/10) (3/5)
                            with { radiusTL = 1/16 }
                        # alignR
                        # lw 0 # fc masonryCl)
                    # alignL)
                # centerXY
            Joe's ->
                let neonSign = text "Joe's" # scale (1/6) # fc neonCl
                        <> rect (1/2) (1/4)
                        # lw 0 # fc miscDarkCl
                in (
                        (
                        neonSign
                        ===
                        vrule (1/4)
                        # lw 0.1 # lc miscDarkCl # lineCap LineCapSquare)
                    # rotateBy (1/4) # alignR # translateY (-1/10)
                    <> (
                        eqTriangle (3/4) # rotateBy (1/4) # scaleX 0.25
                        # lw 0 # fc joesDetailsCl
                        ||| (
                            rect (1/5) (3/10) # alignR
                            # lw 0 # fc joesDetailsCl
                            <> rect (1/3) (3/5) # alignR
                            # lw 0 # fc joesCl)
                       )
                   # alignTR)
               # centerXY
            PlayerGhost ->
                acura playerCl
            OpponentGhost ->
                acura opponentCl
            _ -> mempty


emptySquare = square 1 # lw (1/20)

genericSquare cl = square 1 # lw 0 # fc cl

diagonalTriangle cl = polygon with
    { polyType = PolySides [-3/8 :: Turn]
                           [ 1, sqrt 2 ]
    } # centerXY # lw 0 # fc cl

rightTriangle cl h =
    polygon with
        { polyType = PolySides [1/4 :: Turn]
                               [ h, 1 ]
        , polyOrient = OrientV }
    # alignB # centerX # reflectY # translateY (h / 2)
    # lw 0 # fc cl

getTilePic :: Tile -> CartoM (Diagram BEDia R2)
getTilePic tile =
    let c = getTileChirality tile
        q = getTileOrientation tile
        sf = getTileSurface tile
        et = getElementType tile
    in (\basePic -> basePic
    # reflectByChirality c
    # moveOriginBySize q (getTileSize tile)
    # rotateByOrient q
    ) `liftM` baseElementPic c q sf et
    -- Moving the origin as below seems to work if we don't care about where
    -- the `emptySquare`s of large elements end up.
    -- # moveOriginBySize Q1 (getTileSize tile)

{-# INLINE surfaceToColor #-}
surfaceToColor sf = case sf of
    Tarmac -> tarmacCl
    Dirt -> dirtCl
    Ice -> iceCl

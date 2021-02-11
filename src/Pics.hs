{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Pics
    ( getTerrainPic
    , getTilePic
    ) where

import Control.Monad (liftM)
import Control.Monad.Reader
import Diagrams.Prelude
import Track (Orientation(..), Chirality(..), rotateOrientation
             , ElementType(..), ElementSurface(..), ElementAttribute(..)
             , TerrainType(..)
             , Tile(), getTileOrientation, getTileChirality, getTileSize
             , getTileSurface, getTerrainOrientation
             , getElementType, getTerrainType
             , isElemAttrOf )
import Pics.Palette
import qualified Parameters as Pm
import Pics.MM (acura)
import Types.CartoM
import Types.Diagrams (BEDia)

-- Applies the specified orientation to a base picuture, which should have
-- Q1 orientation and the desired chirality already applied.
--rotateByOrient :: Orientation -> ("Dia" -> "Dia")
rotateByOrient = rotateBy . (/4) . fromIntegral . fromEnum

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

-- Applies the specified chirality to a reference (dextral) picture. This works
-- partly because of the conventions for assigning chiralities and orientations
-- and partly because the chirality is applied to the base picture before the
-- orientation.
--reflectByChirality :: Chirality -> ("Dia" -> "Dia")
reflectByChirality c = case c of
    Sinistral -> reflectY
    _ -> id

baseTerrainPic :: (Monoid' m, TrailLike (QDiagram b V2 Double m))
               => TerrainType -> QDiagram b V2 Double m
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
            # lwG 0.1 # lc hillCl # lineCap LineCapRound)
    AngledMargin ->
        diagonalTriangle waterCl
    OuterAngledSlope ->
        diagonalTriangle slopeCl
    InnerAngledSlope ->
        diagonalTriangle hillCl
        # atop (diagonalTriangle slopeCl # rotateBy (1/2))
    _ ->
        genericSquare chasmCl

-- A minimalistic terrain tile set, similar to the one from dreadnaut's
-- 4DOPEN tool.
twoToneTerrainPic :: (Monoid' m, TrailLike (QDiagram b V2 Double m))
               => TerrainType -> QDiagram b V2 Double m
twoToneTerrainPic tt = case tt of
    Slope ->
        genericSquare hillCl
        # scaleX (1/2) # alignL
    OuterAngledSlope ->
        diagonalTriangle hillCl
        # alignTR
        # scale (1/2)
    InnerAngledSlope ->
        -- Not using diagonalTriangle plainCl here because drawing low
        -- ground diagrams clashes with the transparent low ground
        -- option.
        polygon (with
            & polyType .~ PolySides [-1/4 @@ turn, -1/8 @@ turn, -1/8 @@ turn]
                            [ 1, 1/2, sqrt 2 / 2, 1/2 ]
            ) # centerXY # lwG 0 # fc hillCl
    _ -> baseTerrainPic tt

getTerrainPic :: (Monoid' m, TrailLike (QDiagram b V2 Double m))
               => Bool -> Tile -> QDiagram b V2 Double m
getTerrainPic twoTone tile =
    (if twoTone then twoToneTerrainPic else baseTerrainPic)
        (getTerrainType tile)
    # rotateByOrient (getTerrainOrientation tile)


baseElementPicNoC env = baseElementPic' env Dextral

baseElementPicNoO env = baseElementPicNoC env Q1

-- Note that baseElementPic would work without receiving chiralities or
-- orientations weren't it for the "vertical" offset in the bridge graphics.
baseElementPic :: Chirality -> Orientation
               -> ElementSurface -> ElementType
               -> CartoM (Diagram BEDia)
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
            shearY (corrSignumY q * bridgeH) `underT` translationX 0.5

        rampBaseCorrection q = scaleY (corrSignumY q)

        alignWithRoadY = juxtapose unitY (hrule 1 # translateY (-roadW / 2))

        cornerArc cl w l =
            arc xDir (1/4 @@ turn)
            # alignBL # scale (l - 1/2) # moveOriginBy (r2 (l/2, l/2))
            # lwG w # lc cl

        -- Note this way of assembling a transition leads to minor but
        -- noticeable artefacts if the surrounding triangles have the
        -- same colour as the road between them. That is the reason why
        -- isoscelesTransition is no longer used for the highway
        -- transition.
        isoscelesTransition cl ratio =
            let sidePad = polygon (with
                    & polyType .~ PolySides [1/4 @@ turn]
                                            [ roadW * (ratio - 1) / 2, 1 ]
                    & polyOrient .~ OrientV )
                    # lwG 0 # fc cl # centerXY
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
                    # lwG 0.01 # fc signCl # rotateBy (-1/4))
            SlalomRoad ->
                let slalomBlock = square (slalomRelW * roadW) # scaleX 0.5
                        # translate (r2 (-3/16, -(1 - slalomRelW) * roadW / 2))
                        # lwG 0 # fc blockCl
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
                map p2
                    [ (1/2, roadW * highwayRelW / 2)
                    , (-1/2, roadW / 2)
                    , (-1/2, -roadW / 2)
                    , (1/2, -roadW * highwayRelW / 2)
                    ]
                # fromVertices # closeTrail # strokeTrail
                # centerXY # lwG 0 # fc tarmacCl
                # atop (eqTriangle 1 # alignT
                    # scaleY (sqrt 3 / 3) # scaleX (roadW * hwDivideRelW)
                    # rotateBy (1/4) # lwG 0 # fc highwayCl)
            ElevatedSpan ->
                spanSegment bridgeMeshCl # opacity 0.75
            SpanOverRoad ->
                baseElementPicNoO env sf Road # rotateBy (1/4)
                # atop (baseElementPicNoO env sf ElevatedSpan)
            ElevatedRoad ->
                cat' unitX (with & sep .~ 2 * pillarW) (replicate 3 $
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
                    # shearY (roadW * bankRelH) `underT` translationX (0.5))
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
                    # alignBL # shearX (-1 / (2 * roadW)) # centerXY)
                # atop (fromSegments [ straight (r2 (1/2, roadW)) ]
                    # translate (r2 (-1/4, -roadW / 2))
                    # lwG (corkWallRelW * roadW) # lc warningCl)
                # scaleX 2
            Looping ->
                let loopW = min (loopRelW * roadW) loopMaxW
                    loopB = (loopW - roadW) / 2
                    loopD = loopB / (2 * roadW)
                    loopLeg = baseElementPicNoO env sf Road
                        # shearY loopB `underT` translationX (0.5)
                in centerX (loopLeg ||| loopLeg # rotateBy (1/2))
                # atop (fromOffsets
                    [ r2 (loopD, 0)
                    , r2 (-2 * loopD, -2 * loopB)
                    , r2 (loopD, 0)]
                    # centerXY # lwG roadW # lc meshCl)
            Chicane ->
                fromSegments [ bezier3 (r2 (1, 0)) (r2 (1, -1)) (r2 (2, -1)) ]
                # strokePath # centerXY
                # lwG roadW # lc (surfaceToColor sf)
            CorkUpDown ->
                baseElementPicNoO env sf Road
                # translate (r2 (-0.5, 0.5))
                # atop (circle 0.5 # lwG (bridgeRelW * roadW) # lc bridgeCl)
                # atop (circle 0.5 # lwG roadW # lc tarmacCl)
                # atop (spanSegment tarmacCl
                    # rampCorrection q
                    # reflectByChirality c `underT` translation (r2 (0.5, -bridgeH))
                    # translate (r2 (0.5, 0.5 - bridgeH)))
            Pine ->
                eqTriangle (5/8) # lwG 0.01 # lc darkleafCl # fc leafCl
                ===
                square (1/8) # lwG 0.01 # fc woodCl
            Palm ->
                let leaf = arc (angleDir (1/8 @@ turn)) (3/8 @@ turn)
                        # closeLine # strokeLoop
                        # scale 0.25 # lwG 0.01 # lc darkleafCl # fc leafCl
                in vsep (-31/40)
                    [ square (2/3) # scaleX (1/8) # lwG 0.01 # fc woodCl
                    , (leaf ||| leaf # reflectX) # centerX
                    ]
            Cactus ->
                (
                    fromOffsets [ 0 ^& (-3/16)
                                , (3/8) ^& 0
                                , 0 ^& (1/4)
                                ]
                    # strokePath # centerXY
                    # lineJoin LineJoinRound
                    <> vrule (1/2))
                # lwG 0.15 # lc cactusCl # lineCap LineCapRound
            TennisCourt ->
                (
                    hrule (3/4) # lwG 0.1 # lc netCl
                    <> square (3/4) # lwG 0.01 # fc tennisCl)
                # scaleX 0.5
            Ship ->
                (
                    polygon (with
                        & polyType .~ PolySides [-1/4 @@ turn, -1/8 @@ turn]
                                                [ 1/4, 2/4, sqrt 2 * 1 / 4 ]
                        ) # lwG 0 # fc shipCl
                    ===
                    (vrule (1/2) # lwG 0.01 # lc miscLightCl # alignT
                        ||| strutX (1/10) ||| square (1/5) # alignT
                        # lwG 0 # fc miscDarkCl)
                    # centerX)
                # centerXY
            Barn ->
                let roofArc = wedge (1/2) (angleDir (1/4 @@ turn)) (1/6 @@ turn)
                        # centerY
                    diagLine = hrule (1/3) # rotateBy (1/8)
                in (
                    diagLine <> reflectY diagLine
                    <> arc' (1/6) xDir (1 @@ turn))
                # translateX (1/20) # lwG 0.025 # lc miscLightCl
                <> (
                    (reflectY roofArc <> roofArc) # scaleX 0.5 # scaleY 1.5
                    # lwG 0 # fc barnCl
                    ||| rect (2/5) (1/2) # lc barnCl # fc barnCl)
                # centerXY
            OfficeBuilding ->
                (
                    (
                        atPoints (concat . pathVertices $ rect (1/4) (1/4))
                            (replicate 4 $ square (3/16) # lwG 0  # fc miscDarkCl)
                        ||| strutX (1/16) ||| rect (1/8) (1/4) # lwG 0 # fc miscDarkCl)
                    # alignR
                    <> rect (11/16) (1/2) # alignR # lwG 0 # fc officeCl)
                # centerXY
            Windmill ->
                let blades = hrule (1/2) # rotateBy (1/8)
                        # lwG 0.05 # lc windmillDetailsCl
                    bldg = polygon (with
                        & polyType .~ PolySides
                            [1/4 @@ turn, 1/12 @@ turn, 1/3 @@ turn]
                            [2/5, 3/20, 2/5, 2/5]
                        ) # moveOriginBy (r2 (-(3 * (1 + sqrt 3) / 50),0))
                        # lwG 0 # fc windmillCl
                in reflectY blades <> blades <> bldg
            GasStation ->
                (
                    vrule (3/10) # alignT
                    # lwG 0.1 # lc gasRoofCl # lineCap LineCapRound
                    <> (
                        (
                            rect (3/16) (1/6) # alignR
                            # lwG 0 # fc blueWindowCl
                            ===
                            strutY (1/16)
                            ===
                            rect (1/10) (3/32) # alignR
                            # lwG 0 # fc ethanolCl
                            ===
                            strutY (1/16)
                            ===
                            rect (1/10) (3/32) # alignR
                            # lwG 0 # fc petrolCl)
                        # centerY
                        <> roundedRect' (3/10) (3/5)
                            (with & radiusTL .~ 1/16)
                        # alignR
                        # lwG 0 # fc masonryCl)
                    # alignL)
                # centerXY
            Joe's ->
                let neonSign = text "Joe's" # scale (1/6) # fc neonCl
                        <> rect (3/5) (3/10)
                        # lwG 0 # fc miscDarkCl
                in (
                        (
                        neonSign
                        ===
                        vrule (1/4)
                        # lwG 0.1 # lc miscDarkCl # lineCap LineCapSquare)
                    # rotateBy (1/4) # alignR # translateY (-1/10)
                    <> (
                        eqTriangle (3/4) # rotateBy (1/4) # scaleX 0.25
                        # lwG 0 # fc joesDetailsCl
                        ||| (
                            rect (1/5) (3/10) # alignR
                            # lwG 0 # fc joesDetailsCl
                            <> rect (1/3) (3/5) # alignR
                            # lwG 0 # fc joesCl)
                       )
                   # alignTR)
               # centerXY
            PlayerGhost ->
                acura playerCl
            OpponentGhost ->
                acura opponentCl
            _ -> mempty


emptySquare = square 1 # lwG (1/20)

genericSquare cl = square 1 # lwG 0 # fc cl

diagonalTriangle cl = polygon (with
    & polyType .~ PolySides [-3/8 @@ turn]
                            [ 1, sqrt 2 ]
    ) # centerXY # lwG 0 # fc cl

rightTriangle cl h =
    polygon (with
        & polyType .~ PolySides [1/4 @@ turn]
                                [ h, 1 ]
        & polyOrient .~ OrientV)
    # alignB # centerX # reflectY # translateY (h / 2)
    # lwG 0 # fc cl

getTilePic :: Tile -> CartoM (Diagram BEDia)
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


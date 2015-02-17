{-# LANGUAGE TemplateHaskell #-}
module Annotation.LapTrace
    ( TraceOverlays
    , carsOverTrace
    , periodicCarsSpec

    , TracePoint(..)

    , TraceAnnotation
    , traceAnnPoints
    , traceAnnOverlays
    , traceAnnColour
    , traceAnnVisible

    , putCarOnTracePoint
    , initializeTrace
    , clearOverlays
    ) where

import Control.Lens.Operators hiding ((#))
import qualified Control.Lens as L
import qualified Data.Map as M (Map, fromList, lookup, member, size)
import Data.Default
import Data.List (sortBy, dropWhile, takeWhile)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import Annotation
import Annotation.LapTrace.Vec
import Diagrams.Prelude

type FrameIndex = Int

data TracePoint = TracePoint
    { traceFrame :: FrameIndex
    , tracePosXZ :: (Double, Double)
    , tracePosY :: Double
    , traceRotXZ :: Double
    , traceRotYZ :: Double
    , traceRotXY :: Double
    }

-- TODO: Add support for different overlays.
-- TODO: Add support for multiple periodic series.
data TraceOverlays = TraceOverlays
    { _carsOverTrace :: [(FrameIndex, CarAnnotation)]
    , _periodicCarsSpec :: ((FrameIndex, Int), CarAnnotation)
    }
L.makeLenses ''TraceOverlays

instance Default TraceOverlays where
    def = TraceOverlays
        { _carsOverTrace = []
        -- Initial frame, frequency, base annotation. Only positive
        -- frequencies are supported.
        , _periodicCarsSpec = ((0, 0), defAnn)
        }

instance IsAnnotation TraceOverlays where
    annotation ann = Annotation
        { annotationDiagram =
            mconcat $ map (renderAnnotation . snd) (_carsOverTrace ann)
        }

-- TODO: Implement an option to drop the final frames.
data TraceAnnotation = TraceAnnotation
    { _traceAnnPoints :: [TracePoint]
    , _traceAnnOverlays :: TraceOverlays
    , _traceAnnColour :: Colour Double
    , _traceAnnVisible :: Bool
    }
L.makeLenses ''TraceAnnotation

instance Default TraceAnnotation where
    def = TraceAnnotation
        { _traceAnnPoints = []
        , _traceAnnOverlays = defAnn
        , _traceAnnColour = yellow
        , _traceAnnVisible = True
        }

-- Trace initialization workhorse. Filters out overlays with invalid frames,
-- positions each overlay according to the corresponding trace frame and
-- generates periodic overlays.
setupTrace :: Bool -> TraceAnnotation -> TraceAnnotation
setupTrace isSingleFrame ann = ann & traceAnnOverlays .~ arrangeOverlays tovs
    where
    pointMap = M.fromList $ map (\p -> (traceFrame p, p)) $ ann^.traceAnnPoints
    tovs = (if isSingleFrame then appendPeriodic else id)
        . eliminateFrameless $ ann ^. traceAnnOverlays
    lookupPoint = flip M.lookup pointMap

    arrangeCar (ix, c) =
        let mp = lookupPoint ix
        in (\p -> (ix, putCarOnTracePoint p c)) $
            fromMaybe (error "Frameless overlay.") mp

    arrangeOverlays = L.over carsOverTrace (map arrangeCar)

    eliminateFrameless = L.over carsOverTrace framelessFilter

    framelessFilter = takeWhile (flip M.member pointMap . fst)
        . dropWhile (not . flip M.member pointMap . fst)
        . sortBy (comparing fst)

    lastFrame = M.size pointMap - 1
    appendPeriodic tovs =
        let ((ifr, freq), baseCar) = tovs ^. periodicCarsSpec
            ifr' = max 0 ifr
        in L.over carsOverTrace (++
            if freq > 0 then
                zip [ifr', ifr' + freq .. lastFrame] $ repeat baseCar
            else
                []
            ) tovs

-- 20fps assumed throughout. Note that this implementation
-- only makes sense for positive values.
formatFrameAsGameTime :: Int -> String
formatFrameAsGameTime x = (if mm > 0 then show mm ++ ":" else "")
    ++ printf "%02d" ss ++ "." ++ printf "%02d" cc
    where
    (sm, cc) = (5 * x) `quotRem` 100
    (mm, ss) = sm `quotRem` 60

replaceMagicStringsForCar :: Int -> CarAnnotation -> CarAnnotation
replaceMagicStringsForCar ix c =
    let txt = c ^. carAnnCaption . captAnnText
    in case txt of
        "{{FRAMENUMBER}}" ->
            L.over carAnnCaption (captAnnText .~ show ix) c
        "{{GAMETIME}}" ->
            L.over carAnnCaption (captAnnText .~ formatFrameAsGameTime ix) c
        _ -> c

putCarOnTracePoint :: TracePoint -> CarAnnotation -> CarAnnotation
putCarOnTracePoint p =
    replaceMagicStringsForCar (traceFrame p)
    . scaleAnnotation (1 + tracePosY p / 4)
    . orientAnnotation (traceRotXZ p)
    . locateAnnotation (tracePosXZ p)

tracePointsFromData :: [(VecDouble, VecDouble)] -> [TracePoint]
tracePointsFromData dat = map mkPoint $ zip [0..] dat
    where
    mkPoint (ix, ((px, py, pz), (rxz, ryz, rxy))) = TracePoint
        { traceFrame = ix
        , tracePosXZ = (px, pz)
        , tracePosY = py
        , traceRotXZ = rxz
        , traceRotYZ = ryz
        , traceRotXY = rxy
        }

initializeTrace :: Bool -> [(VecDouble, VecDouble)]
                -> TraceAnnotation -> TraceAnnotation
initializeTrace isSingleFrame dat =
    setupTrace isSingleFrame . setTraceData dat

setTraceData :: [(VecDouble, VecDouble)]
             -> TraceAnnotation -> TraceAnnotation
setTraceData dat = (traceAnnPoints .~ tracePointsFromData dat)

clearOverlays :: TraceAnnotation -> TraceAnnotation
clearOverlays = traceAnnOverlays .~ defAnn

-- TODO: Add a LocatableAnnotation instance (obviously it will relocate the
-- overlays too).

instance IsAnnotation TraceAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            (renderAnnotation $ ann ^. traceAnnOverlays)
            <>
            if ann ^. traceAnnVisible then
                fromVertices (map (p2 . tracePosXZ) $ ann ^. traceAnnPoints)
                # lc (ann ^. traceAnnColour) # lwG 0.05
            else
                mempty
        }

instance ColourAnnotation TraceAnnotation where
    annColour = _traceAnnColour
    setAnnColour = (traceAnnColour .~)
    annColourIsProtected = const False -- TODO: Not implemented yet.
    protectAnnColour ann = ann
    deepOverrideAnnColour cl ann = overrideAnnColour cl ann
        & L.over traceAnnOverlays
                ( L.over carsOverTrace (map (fmap $ deepOverrideAnnColour cl))
                . L.over periodicCarsSpec (fmap (deepOverrideAnnColour cl))
                )

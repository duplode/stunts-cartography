module Annotation.LapTrace
    ( TraceOverlays(..)
    , TracePoint(..)
    , TraceAnnotation(..)
    , putCarOnTracePoint
    , initializeTrace
    , clearOverlays
    ) where

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
    { carsOverTrace :: [(FrameIndex, CarAnnotation)]
    , periodicCarsSpec :: ((FrameIndex, Int), CarAnnotation)
    }

instance Default TraceOverlays where
    def = TraceOverlays
        { carsOverTrace = []
        -- Initial frame, frequency, base annotation. Only positive
        -- frequencies are supported.
        , periodicCarsSpec = ((0, 0), defAnn)
        }

instance IsAnnotation TraceOverlays where
    annotation ann = Annotation
        { annotationDiagram =
            mconcat $ map (renderAnnotation . snd) (carsOverTrace ann)
        }

-- TODO: Implement an option to drop the final frames.
data TraceAnnotation = TraceAnnotation
    { traceAnnPoints :: [TracePoint]
    , traceAnnOverlays :: TraceOverlays
    , traceAnnColour :: Colour Double
    , traceAnnVisible :: Bool
    }

instance Default TraceAnnotation where
    def = TraceAnnotation
        { traceAnnPoints = []
        , traceAnnOverlays = defAnn
        , traceAnnColour = yellow
        , traceAnnVisible = True
        }

-- Trace initialization workhorse. Filters out overlays with invalid frames,
-- positions each overlay according to the corresponding trace frame and
-- generates periodic overlays.
setupTrace :: Bool -> TraceAnnotation -> TraceAnnotation
setupTrace isSingleFrame ann = ann
    { traceAnnOverlays = arrangeOverlays tovs
    }
    where
    pointMap = M.fromList $ map (\p -> (traceFrame p, p)) $ traceAnnPoints ann
    tovs = (if isSingleFrame then appendPeriodic else id)
        . eliminateFrameless $ traceAnnOverlays ann
    lookupPoint = flip M.lookup pointMap

    arrangeCar (ix, c) =
        let mp = lookupPoint ix
        in (\p -> (ix, putCarOnTracePoint p c)) $
            fromMaybe (error "Frameless overlay.") mp

    arrangeOverlays tovs = tovs
        { carsOverTrace = map arrangeCar $ carsOverTrace tovs
        }

    eliminateFrameless tovs = tovs
        { carsOverTrace = framelessFilter $ carsOverTrace tovs
        }
    framelessFilter = takeWhile (flip M.member pointMap . fst)
        . dropWhile (not . flip M.member pointMap . fst)
        . sortBy (comparing fst)

    lastFrame = M.size pointMap - 1
    appendPeriodic tovs =
        let ((ifr, freq), baseCar) = periodicCarsSpec tovs
            ifr' = max 0 ifr
        in tovs
            { carsOverTrace = carsOverTrace tovs ++
                if freq > 0 then
                    zip [ifr', ifr' + freq .. lastFrame] $ repeat baseCar
                else
                    []
            }

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
    let txt = captAnnText . carAnnCaption $ c
    in case txt of
        "{{FRAMENUMBER}}" ->
            c { carAnnCaption = (carAnnCaption c)
                { captAnnText = show ix }}
        "{{GAMETIME}}" ->
            c { carAnnCaption = (carAnnCaption c)
                { captAnnText = formatFrameAsGameTime ix }}
        _ -> c

putCarOnTracePoint :: TracePoint -> CarAnnotation -> CarAnnotation
putCarOnTracePoint p =
    replaceMagicStringsForCar (traceFrame p)
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
setTraceData dat ann = ann { traceAnnPoints = tracePointsFromData dat }

clearOverlays :: TraceAnnotation -> TraceAnnotation
clearOverlays ann = ann { traceAnnOverlays = defAnn }

-- TODO: Add a LocatableAnnotation instance (obviously it will relocate the
-- overlays too).

instance IsAnnotation TraceAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            (renderAnnotation $ traceAnnOverlays ann)
            <>
            if traceAnnVisible ann then
                fromVertices (map (p2 . tracePosXZ) . traceAnnPoints $ ann)
                # lc (traceAnnColour ann) # lw 0.05
            else
                mempty
        }

instance ColourAnnotation TraceAnnotation where
    annColour = traceAnnColour
    setAnnColour cl ann = ann { traceAnnColour = cl }
    annColourIsProtected = const False -- TODO: Not implemented yet.
    protectAnnColour ann = ann
    deepOverrideAnnColour cl ann =
        let tovs = traceAnnOverlays ann
        in overrideAnnColour cl ann
            { traceAnnOverlays = tovs
                { carsOverTrace = map (fmap $ deepOverrideAnnColour cl) $
                    carsOverTrace tovs
                , periodicCarsSpec = fmap (deepOverrideAnnColour cl) $
                    periodicCarsSpec tovs
                }
            }


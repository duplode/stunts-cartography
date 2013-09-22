module Annotation.LapTrace
    ( TraceOverlays(..)
    , emptyTraceOverlays
    , TraceAnnotation(..)
    , emptyTraceAnn
    , initializeTrace
    ) where

import qualified Data.Map as M

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
data TraceOverlays = TraceOverlays
    { carsOverTrace :: [(FrameIndex, CarAnnotation)]
    }

emptyTraceOverlays :: TraceOverlays
emptyTraceOverlays = TraceOverlays
    { carsOverTrace = []
    }

-- TODO: Curb the massive ammounts of boilerplate to render an annotation.
instance IsAnnotation TraceOverlays where
    annotation ann = Annotation
        { renderAnnotation =
            mconcat $ map
                (renderAnnotation . annotation . snd) (carsOverTrace ann)
        }

-- TODO: Implement an option to drop the final frames.
data TraceAnnotation = TraceAnnotation
    { traceAnnPoints :: [TracePoint]
    , traceAnnOverlays :: TraceOverlays
    , traceAnnColour :: Colour Double
    , traceAnnVisible :: Bool
    }

emptyTraceAnn :: TraceAnnotation
emptyTraceAnn = TraceAnnotation
    { traceAnnPoints = []
    , traceAnnOverlays = emptyTraceOverlays
    , traceAnnColour = yellow
    , traceAnnVisible = True
    }

arrangeOverlays :: TraceAnnotation -> TraceAnnotation
arrangeOverlays ann = ann { traceAnnOverlays = arrangeOverlays' tovs }
    where
    tovs = traceAnnOverlays ann
    pointMap = M.fromList $ map (\p -> (traceFrame p, p)) $ traceAnnPoints ann
    lookupPoint = flip M.lookup pointMap

    arrangeCar (ix, c) =
        let mp = lookupPoint ix
            fArrange p = orientAnnotation (traceRotXZ p)
                . locateAnnotation (tracePosXZ p)
        -- TODO: This is not a nice way to handle a missing index.
        in maybe (ix, c) (\p -> (ix, fArrange p c)) mp

    arrangeOverlays' tovs = tovs
        { carsOverTrace = map arrangeCar $ carsOverTrace tovs
        }

-- TODO: Add regularly spaced cars.

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

initializeTrace :: [(VecDouble, VecDouble)]
                -> TraceAnnotation -> TraceAnnotation
initializeTrace dat ann = arrangeOverlays
    ann { traceAnnPoints = tracePointsFromData dat }

-- TODO: Add a LocatableAnnotation instance (obviously it will relocate the
-- overlays too).

instance IsAnnotation TraceAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            (renderAnnotation . annotation $ traceAnnOverlays ann)
            <>
            if traceAnnVisible ann then
                fromVertices (map (p2 . tracePosXZ) . traceAnnPoints $ ann)
                # lc (traceAnnColour ann) # lw 0.05
            else
                mempty
        }


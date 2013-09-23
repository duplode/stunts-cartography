module Annotation.LapTrace
    ( TraceOverlays(..)
    , TraceAnnotation(..)
    , initializeTrace
    ) where

import qualified Data.Map as M
import Data.Default

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

instance Default TraceOverlays where
    def = TraceOverlays
        { carsOverTrace = []
        }

-- TODO: Curb the massive ammounts of boilerplate to render an annotation.
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

-- TraceAnnotation is not a ColourAnnotation instance for two flimsy reasons.
-- Firstly, it is not essential to do so, as lap traces are, at least for the
-- current use cases, top-level annotations. Secondly, skipping the instance
-- allows us to defer the decision on whether all overlays should be instances
-- of ColourAnnotation as well.

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
        { annotationDiagram =
            (renderAnnotation $ traceAnnOverlays ann)
            <>
            if traceAnnVisible ann then
                fromVertices (map (p2 . tracePosXZ) . traceAnnPoints $ ann)
                # lc (traceAnnColour ann) # lw 0.05
            else
                mempty
        }


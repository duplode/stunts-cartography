{-# LANGUAGE TemplateHaskell #-}
module Annotation.LapTrace
    ( TraceOverlays
    , carsOverTrace
    , periodicCarsSpec

    , PeriodicCarsSpec
    , periodicInitialFrame
    , periodicPeriod
    , periodicBaseCar
    , periodicFlipbookCaption

    , TracePoint(..)
    , TraceMode(..)

    , TraceAnnotation
    , traceAnnPoints
    , traceAnnOverlays
    , traceAnnColour
    , traceAnnVisible

    , putCarOnTracePoint
    , initializeTrace
    , clearOverlays
    , replaceMagicStrings
    ) where

import Control.Lens.Operators hiding ((#))
import qualified Control.Lens as L
import qualified Data.Map as M (Map, fromList, lookup, member, size)
import Data.Default
import Data.List (sortBy, dropWhile, takeWhile)
import Data.List.Extra (replace)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Numeric.Extra (showDP)

import Annotation
import Annotation.LapTrace.Vec (PreTracePoint(..))
import Diagrams.Prelude

type FrameIndex = Int

data TracePoint = TracePoint
    { traceFrame :: FrameIndex
    , tracePosXZ :: (Double, Double)
    , tracePosY :: Double
    , traceRotXZ :: Double
    , traceRotYZ :: Double
    , traceRotXY :: Double
    , traceSpeed :: Maybe Double
    , traceGear :: Maybe Int
    }

data TraceMode = SingleFrameTrace | FlipbookTrace
    deriving (Eq, Enum)

data PeriodicCarsSpec = PeriodicCarsSpec
   { _periodicInitialFrame :: FrameIndex
   , _periodicPeriod :: Int
   , _periodicBaseCar :: CarAnnotation
   -- This caption is only shown when the trace is rendered as a
   -- flipbook. It is subject to magic string replacement.
   , _periodicFlipbookCaption :: CaptAnnotation
   }
L.makeLenses ''PeriodicCarsSpec

instance Default PeriodicCarsSpec where
    def = PeriodicCarsSpec
       { _periodicInitialFrame = 0
       , _periodicPeriod = 0
       , _periodicBaseCar = defAnn
       , _periodicFlipbookCaption = defAnn
       }

-- TODO: Add support for different overlays.
-- TODO: Add support for multiple periodic series.
data TraceOverlays = TraceOverlays
    { _carsOverTrace :: [(FrameIndex, CarAnnotation)]
    , _periodicCarsSpec :: PeriodicCarsSpec
    }
L.makeLenses ''TraceOverlays

instance Default TraceOverlays where
    def = TraceOverlays
        { _carsOverTrace = []
        -- Initial frame, frequency, base annotations. Only positive
        -- frequencies are supported.
        , _periodicCarsSpec = def
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
setupTrace :: TraceMode -> TraceAnnotation -> TraceAnnotation
setupTrace traceMode ann = ann & traceAnnOverlays .~ arrangeOverlays tovs
    where
    pointMap = M.fromList $ map (\p -> (traceFrame p, p)) $ ann^.traceAnnPoints
    tovs = (case traceMode of
            SingleFrameTrace -> appendPeriodic
            FlipbookTrace -> id)
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
    -- The flipbook caption is not used here.
    appendPeriodic tovs =
        let pcSpec = tovs ^. periodicCarsSpec
            ifr = pcSpec ^. periodicInitialFrame
            freq = pcSpec ^. periodicPeriod
            baseCar = pcSpec ^. periodicBaseCar
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

formatSpeed :: Double -> String
formatSpeed = showDP 1

replaceMagicStrings :: TextAnnotation a => TracePoint -> a -> a
replaceMagicStrings p c =
    let txt = c ^. annText
        ix = traceFrame p
    in c & annText %~ replace "{{FRAMENUMBER}}" (show ix)
        . replace "{{GAMETIME}}" (formatFrameAsGameTime ix)
        . maybe id (replace "{{SPEED}}" . formatSpeed) (traceSpeed p)
        . maybe id (replace "{{GEAR}}" . show) (traceGear p)

putCarOnTracePoint :: TracePoint -> CarAnnotation -> CarAnnotation
putCarOnTracePoint p =
    replaceMagicStrings p
    . scaleAnnotation (1 + tracePosY p / 4)
    . (annAngle .~ traceRotXZ p)
    . (annPosition .~ tracePosXZ p)

tracePointsFromData :: [PreTracePoint] -> [TracePoint]
tracePointsFromData dat = map mkPoint $ zip [0..] dat
    where
    mkPoint (ix, ptp) = TracePoint
        { traceFrame = ix
        , tracePosXZ = (fst3 (preTracePos ptp), thd3 (preTracePos ptp))
        , tracePosY = snd3 (preTracePos ptp)
        , traceRotXZ = fst3 (preTraceRot ptp)
        , traceRotYZ = snd3 (preTraceRot ptp)
        , traceRotXY = thd3 (preTraceRot ptp)
        , traceSpeed = preTraceSpeed ptp
        , traceGear = preTraceGear ptp
        }

initializeTrace :: TraceMode -> [PreTracePoint]
                -> TraceAnnotation -> TraceAnnotation
initializeTrace traceMode dat =
    setupTrace traceMode . setTraceData dat

setTraceData :: [PreTracePoint] -> TraceAnnotation -> TraceAnnotation
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

-- TODO: Double-check how overriding works in complex cases such as this one.
instance ColourAnnotation TraceAnnotation where
    annColour = traceAnnColour
    annColourIsProtected = const False -- TODO: Not implemented yet.
    protectAnnColour ann = ann
    deepOverrideAnnColour cl ann = overrideAnnColour cl ann
        & L.over traceAnnOverlays
                ( L.over carsOverTrace (map (fmap $ deepOverrideAnnColour cl))
                . L.over (periodicCarsSpec . periodicBaseCar) (deepOverrideAnnColour cl)
                . L.over (periodicCarsSpec . periodicFlipbookCaption)
                    (deepOverrideAnnColour cl)
                )

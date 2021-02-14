{-# LANGUAGE TemplateHaskell #-}
module Annotation.LapTrace
    ( FrameIndex
    , TraceOverlays
    , carsOverTrace
    , periodicCarsSpec

    , PeriodicCarsSpec
    , periodicInitialFrame
    , periodicPeriod
    , periodicBaseCar
    --, periodicStandaloneCaptions
    , periodicFlipbookCaptions

    , TracePoint(..)
    , TraceMode(..)

    , TraceAnnotation
    , traceAnnPoints
    , traceAnnOverlays
    , traceAnnColour
    , traceAnnVisible
    , traceAnnWidth

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
   -- Standlone frame-bound captions associated to periodic annotations
   -- are ignored, because rendering them on a single picture would put
   -- captions for different frames on the top of each other. For that
   -- reason, flipbook standalone frame-bound captions are parsed,
   -- processed and rendered separately
   --, _periodicStandaloneCaptions :: [CaptAnnotation]
   -- These captions are only shown when the trace is rendered as a
   -- flipbook.
   , _periodicFlipbookCaptions :: [CaptAnnotation]
   }
L.makeLenses ''PeriodicCarsSpec

instance Default PeriodicCarsSpec where
    def = PeriodicCarsSpec
       { _periodicInitialFrame = 0
       , _periodicPeriod = 0
       , _periodicBaseCar = defAnn
       --, _periodicStandaloneCaptions = []
       , _periodicFlipbookCaptions = []
       }

-- TODO: Add support for different overlays.
data TraceOverlays = TraceOverlays
    -- Triple components: frame index, base sprite annotation, and
    -- frame-bound standalone captions. The triple should be
    -- replaced by a less anonymous type eventually.
    { _carsOverTrace :: [(FrameIndex, CarAnnotation, [CaptAnnotation])]
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
            let renderFrame (_, baseCar, saCapts) =
                    renderAnnotation baseCar <> foldMap renderAnnotation saCapts
            in foldMap renderFrame (_carsOverTrace ann)
        }

-- TODO: Implement an option to drop the final frames.
data TraceAnnotation = TraceAnnotation
    { _traceAnnPoints :: [TracePoint]
    , _traceAnnOverlays :: TraceOverlays
    , _traceAnnColour :: Colour Double
    , _traceAnnWidth :: Double
    , _traceAnnVisible :: Bool
    }
L.makeLenses ''TraceAnnotation

instance Default TraceAnnotation where
    def = TraceAnnotation
        { _traceAnnPoints = []
        , _traceAnnOverlays = defAnn
        , _traceAnnColour = yellow
        , _traceAnnWidth = 0.05
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

    -- A few notes:
    --
    -- * This doesn't blow up at runtime only because there is a
    -- previous eliminateFrameless pass beforehand.
    --
    -- * Frameless decorations are, as it stands, dropped silently.
    --
    -- * Periodic overlays don't need an eliminateFrameless pass, as
    -- appendPeriodic does not generate frame indices past the end of
    -- the trace.
    arrangeCar (ix, c, saCapts) =
        let mp = lookupPoint ix
        in (\p -> (ix
                , putCarOnTracePoint p c
                , map (replaceMagicStrings p) saCapts)) $
            fromMaybe (error "Frameless overlay.") mp

    arrangeOverlays = L.over carsOverTrace (map arrangeCar)

    eliminateFrameless = L.over carsOverTrace framelessFilter

    framelessFilter = takeWhile (flip M.member pointMap . view _1)
        . dropWhile (not . flip M.member pointMap . view _1)
        . sortBy (comparing (view _1))

    lastFrame = M.size pointMap - 1
    -- The flipbook captions are not used here. They are only rendered
    -- by toFlipbook @TraceAnnotation.
    appendPeriodic tovs =
        let pcSpec = tovs ^. periodicCarsSpec
            ifr = pcSpec ^. periodicInitialFrame
            freq = pcSpec ^. periodicPeriod
            baseCar = pcSpec ^. periodicBaseCar
            --saCapts = pcSpec ^. periodicStandaloneCaptions
            ifr' = max 0 ifr
            -- Standalone frame-bound captions are not used here.
            --mkTriple ix = (ix, baseCar, saCapts)
            mkTriple ix = (ix, baseCar, [])
        in L.over carsOverTrace (++
            if freq > 0 then
                map mkTriple [ifr', ifr' + freq .. lastFrame]
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

-- Exact tile to metre conversion.
formatLengthInMetres :: Double -> String
formatLengthInMetres = showDP 2 . (62.42304 *)

replaceMagicStrings :: TextAnnotation a => TracePoint -> a -> a
replaceMagicStrings p c =
    let txt = c ^. annText
        ix = traceFrame p
    in c & annText %~ replace "{{FRAMENUMBER}}" (show ix)
        . replace "{{GAMETIME}}" (formatFrameAsGameTime ix)
        . replace "{{HEIGHT}}" (formatLengthInMetres (tracePosY p))
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
        , tracePosXZ = (preTracePos ptp ^. _1, preTracePos ptp ^. _3)
        , tracePosY = preTracePos ptp ^. _2
        , traceRotXZ = preTraceRot ptp ^. _1
        , traceRotYZ = preTraceRot ptp ^. _2
        , traceRotXY = preTraceRot ptp ^. _3
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
                # lc (ann ^. traceAnnColour) # lwG (ann ^. traceAnnWidth)
            else
                mempty
        }

-- TODO: Double-check how overriding works in complex cases such as this one.
instance ColourAnnotation TraceAnnotation where
    annColour = traceAnnColour
    annColourIsProtected = const False -- TODO: Not implemented yet.
                                       -- For now, a trace is always a
                                       -- top-level annotation, so we
                                       -- don't need protection.
    protectAnnColour ann = ann
    deepOverrideAnnColour cl ann = overrideAnnColour cl ann
        & L.over traceAnnOverlays
                ( L.over (carsOverTrace . mapped . _2) (deepOverrideAnnColour cl)
                . L.over (carsOverTrace . mapped . _3 . mapped) (deepOverrideAnnColour cl)
                . L.over (periodicCarsSpec . periodicBaseCar) (deepOverrideAnnColour cl)
                . L.over (periodicCarsSpec . periodicFlipbookCaptions . mapped)
                    (deepOverrideAnnColour cl)
                )

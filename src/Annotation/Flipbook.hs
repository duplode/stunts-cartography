module Annotation.Flipbook
    ( ToFlipbook (toFlipbook)
    ) where

import Diagrams.Prelude

import Types.Diagrams
import Annotation
import Annotation.LapTrace

class (IsAnnotation a) => ToFlipbook a where
    toFlipbook :: a -> [Diagram BEDia R2]

-- Annotations.LapTrace.setupTrace is not supposed to be employed before
-- using this instance.
instance ToFlipbook TraceAnnotation where
    -- TODO: envelop.
    toFlipbook ann =
        map (renderAnnotation . flip putCarOnTracePoint baseCar) pts
        where
        ((ifr, freq), baseCar) = periodicCarsSpec . traceAnnOverlays $ ann
        pts = traceAnnPoints ann


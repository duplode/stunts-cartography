{-# LANGUAGE ExistentialQuantification #-}
module Annotation.Flipbook
    ( ToFlipbook (toFlipbook, flipbookBackdrop, renderFlipbook)
    , SomeFlipbook(..)
    ) where

import Diagrams.Prelude

import Types.Diagrams
import Annotation
import Annotation.LapTrace

class ToFlipbook a where
    toFlipbook :: a -> [Diagram BEDia R2]
    flipbookBackdrop :: a -> Diagram BEDia R2
    renderFlipbook :: a -> (Diagram BEDia R2, [Diagram BEDia R2])

    renderFlipbook ann = (flipbookBackdrop ann, toFlipbook ann)

-- Annotations.LapTrace.setupTrace is not supposed to be employed before
-- using this instance.
instance ToFlipbook TraceAnnotation where
    toFlipbook ann =
        let ((ifr, freq), baseCar) = periodicCarsSpec . traceAnnOverlays $ ann
            pointIsIncluded p =
                let phaselessFrame = traceFrame p - ifr
                in phaselessFrame >= 0 && phaselessFrame `rem` freq == 0
            fRenderOn c p =
                if pointIsIncluded p
                    then renderAnnotation $ putCarOnTracePoint p c
                    else mempty
            pts = traceAnnPoints ann
        in map (fRenderOn baseCar) pts
    flipbookBackdrop = renderAnnotation


-- Generic concrete instance.
data RenderedFlipbook = RenderedFlipbook
    { renderedFlipbookPages :: [Diagram BEDia R2]
    , renderedFlipbookBackdrop :: Diagram BEDia R2
    }

instance ToFlipbook RenderedFlipbook where
    toFlipbook = renderedFlipbookPages
    flipbookBackdrop = renderedFlipbookBackdrop

-- Existential wrapper.

data SomeFlipbook = forall a. ToFlipbook a => SomeFlipbook a

instance ToFlipbook SomeFlipbook where
    toFlipbook (SomeFlipbook ann) = toFlipbook ann
    flipbookBackdrop (SomeFlipbook ann) = flipbookBackdrop ann

instance Monoid SomeFlipbook where
    mempty = SomeFlipbook $ RenderedFlipbook
        { renderedFlipbookPages = []
        , renderedFlipbookBackdrop = mempty
        }
    mappend (SomeFlipbook ann1) (SomeFlipbook ann2) =
        SomeFlipbook $ RenderedFlipbook
            { renderedFlipbookPages =
                zipStillLastWith mappend (toFlipbook ann1) (toFlipbook ann2)
            , renderedFlipbookBackdrop =
                mappend (flipbookBackdrop ann1) (flipbookBackdrop ann2)
            }

-- Zipping so that the final length is that of the longer list. The shorter
-- list has its last element repeated to make it fit. That mirrors the behavior
-- of the Stunts engine when one car finishes ahead of the other in a race.
zipStillLastWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipStillLastWith _ [] ys          = ys
zipStillLastWith _ xs []          = xs
zipStillLastWith f [x] ys         = zipWith f (repeat x) ys
zipStillLastWith f xs [y]         = zipWith f xs (repeat y)
zipStillLastWith f (x:xs) (y:ys)  = f x y : zipStillLastWith f xs ys

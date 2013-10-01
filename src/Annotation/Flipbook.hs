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
            pts = traceAnnPoints ann
        in map (renderAnnotation . flip putCarOnTracePoint baseCar) pts
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
                zipStillTailWith mappend (toFlipbook ann1) (toFlipbook ann2)
            , renderedFlipbookBackdrop =
                mappend (flipbookBackdrop ann1) (flipbookBackdrop ann2)
            }
        where
        zipPages pgs1 [] = pgs1
        zipPages [] pgs2 = pgs2

zipStillTailWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipStillTailWith _ [] ys          = ys
zipStillTailWith _ xs []          = xs
zipStillTailWith f [x] ys         = zipWith f (repeat x) ys
zipStillTailWith f xs [y]         = zipWith f xs (repeat y)
zipStillTailWith f (x:xs) (y:ys)  = f x y : zipStillTailWith f xs ys

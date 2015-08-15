{-# LANGUAGE ExistentialQuantification #-}
module Annotation.Flipbook
    ( ToFlipbook (toFlipbook, flipbookBackdrop)
    , SomeFlipbook(..)
    , zipFlipbookPages
    , concatFlipbookBackdrops
    ) where

import Data.List (groupBy)
import Diagrams.Prelude

import Types.Diagrams
import Annotation
import Annotation.LapTrace

class ToFlipbook a where
    toFlipbook :: a -> [Diagram BEDia]
    flipbookBackdrop :: a -> Diagram BEDia

-- Annotations.LapTrace.initializeTrace is supposed to be called with the
-- single frame option disabled before using this instance.
instance ToFlipbook TraceAnnotation where
    toFlipbook ann =
        let ((ifr, freq), baseCar) = ann ^. traceAnnOverlays . periodicCarsSpec
            pointIsIncluded p =
                let phaselessFrame = traceFrame p - ifr
                in phaselessFrame >= 0 && phaselessFrame `rem` freq == 0
            fRenderOn c p =
                if pointIsIncluded p
                    then (True, renderAnnotation $ putCarOnTracePoint p c)
                    else (False, mempty)
            pts = ann ^. traceAnnPoints
        -- Note that freq should actually be called period.
        in case freq of
            0 -> [] -- TODO: Notify the issue (through the parser, probably).
            1 -> map (snd . fRenderOn baseCar) pts
            _ -> spillOverEmpty $ map (fRenderOn baseCar) pts
    flipbookBackdrop = renderAnnotation

-- Frame replication to account for frame rate variations.
spillOverEmpty :: [(Bool, Diagram BEDia)] -> [Diagram BEDia]
spillOverEmpty = concatMap (overwriteWithHead . map snd)
    . groupBy (((not . fst) .) . flip const)
    where
    overwriteWithHead [] = []
    overwriteWithHead (x:xs) = x : map (const x) xs

-- Generic concrete instance.
data RenderedFlipbook = RenderedFlipbook
    { renderedFlipbookPages :: [Diagram BEDia]
    , renderedFlipbookBackdrop :: Diagram BEDia
    }

instance ToFlipbook RenderedFlipbook where
    toFlipbook = renderedFlipbookPages
    flipbookBackdrop = renderedFlipbookBackdrop

-- Existential wrapper.
data SomeFlipbook = forall a. ToFlipbook a => SomeFlipbook a

instance ToFlipbook SomeFlipbook where
    toFlipbook (SomeFlipbook ann) = toFlipbook ann
    flipbookBackdrop (SomeFlipbook ann) = flipbookBackdrop ann

-- A less complex alternative to the monoid instance for the common use case.
-- The signatures can be more general, if the need arises.
zipFlipbookPages :: [SomeFlipbook] -> [Diagram BEDia]
zipFlipbookPages = foldr (zipStillLastWith mappend . toFlipbook) []

concatFlipbookBackdrops :: [SomeFlipbook] -> Diagram BEDia
concatFlipbookBackdrops = mconcat . map flipbookBackdrop

-- Zipping so that the final length is that of the longer list. The shorter
-- list has its last element repeated to make it fit. That mirrors the behavior
-- of the Stunts engine when one car finishes ahead of the other in a race.
zipStillLastWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipStillLastWith _ [] ys          = ys
zipStillLastWith _ xs []          = xs
zipStillLastWith f [x] ys         = zipWith f (repeat x) ys
zipStillLastWith f xs [y]         = zipWith f xs (repeat y)
zipStillLastWith f (x:xs) (y:ys)  = f x y : zipStillLastWith f xs ys

-- The monoid instance is easy on the eyes when used; however, the conversion to
-- an intermediate wrapper can complicate things.
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


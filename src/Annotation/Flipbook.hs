module Annotation.Flipbook
    ( Flipbook(..)
    , ToFlipbook (toFlipbook)
    , underlayFlipbook
    , renderFlipbook
    ) where

import Data.Default
import Diagrams.Prelude

import Types.Diagrams
import Annotation
import Annotation.LapTrace

-- TODO: Specify frame rate.
data Flipbook = Flipbook
    { flipbookPages :: [Diagram BEDia R2]
    , flipbookBackdrop :: Diagram BEDia R2
    }

instance Default Flipbook where
    def = Flipbook
        { flipbookPages = []
        , flipbookBackdrop = mempty
        }

class ToFlipbook a where
    toFlipbook :: a -> Flipbook

instance ToFlipbook TraceAnnotation where
    toFlipbook ann =
        let tovs = traceAnnOverlays ann
        in Flipbook
            { flipbookPages = map (renderAnnotation . snd) $ carsOverTrace tovs
            , flipbookBackdrop = renderAnnotation $ clearOverlays ann
            }

-- TODO: It is not fully clear whether this is sensible.
instance (IsAnnotation a) => ToFlipbook [a] where
    toFlipbook anns = def
        { flipbookPages = map renderAnnotation anns
        }

underlayFlipbook :: Diagram BEDia R2 -> Flipbook -> Flipbook
underlayFlipbook bd fbk = fbk
    { flipbookBackdrop = flipbookBackdrop fbk <> bd
    }

-- Use this function to obtain the resulting diagrams rather than accessing
-- the fields directly, as it performs some extra processing.
renderFlipbook :: Flipbook -> (Diagram BEDia R2, [Diagram BEDia R2])
renderFlipbook fbk =
    let backdrop = flipbookBackdrop fbk
    -- TODO: Test this implementation.
    in (backdrop, map (withEnvelope backdrop) $ flipbookPages fbk)

-- TODO: Is this necessary?
-- mkAnimation = [Diagram BEDia R2] -> Animation BEDia R2

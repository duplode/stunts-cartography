{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Annotation
    ( CardinalDirection(..)
    , Annotation(..)
    , IsAnnotation
    , annotation
    , LocatableAnnotation
    , annPosition
    , locateAnnotation
    , translateAnnotation
    , OrientableAnnotation
    , annAngle
    , orientAnnotation
    , rotateAnnotation
    , CarAnnotation(..)
    , SegAnnotation(..)
    , SplitAnnotation(..)
    , CaptAnnotation(..)
    ) where

-- It might be sensible to import this qualified if you need the constructors.

import Diagrams.Prelude
import qualified Diagrams.Backend.Cairo.Text as CairoText
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV
import Types.Diagrams (BEDia)
import Pics.MM

data CardinalDirection = E
                       | N
                       | W
                       | S
                       deriving (Read, Show, Eq, Ord)

data Annotation
    = Annotation
    { renderAnnotation :: Diagram BEDia R2
    }

class IsAnnotation a where
    annotation :: a -> Annotation

class (IsAnnotation a) => LocatableAnnotation a where
    annPosition :: a -> (Double, Double)
    locateAnnotation :: (Double, Double) -> a -> a
    translateAnnotation :: (Double, Double) -> a -> a

    translateAnnotation (dx, dy) ann =
        let (xi, yi) = annPosition ann
        in locateAnnotation (xi + dx, yi + dy) ann

-- Angles are assumed to be in degrees, for simplicity.
class (IsAnnotation a) => OrientableAnnotation a where
    annAngle :: a -> Double
    orientAnnotation :: Double -> a -> a
    rotateAnnotation :: Double -> a -> a

    rotateAnnotation da ann = orientAnnotation (da + annAngle ann) ann

data CarAnnotation
     = CarAnnotation
     { carAnnColour :: Colour Double
     , carAnnPosition :: (Double, Double)
     , carAnnAngle :: Double
     , carAnnSize :: Double
     , carAnnCaption :: CaptAnnotation
     } deriving (Show)

instance IsAnnotation CarAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            acura' (carAnnColour ann) 1
            # scale (carAnnSize ann)
            # (flip $ beside
                (cardinalDirToR2 . captAnnAlignment . carAnnCaption $ ann))
                (renderAnnotation . annotation
                    . rotateAnnotation (- carAnnAngle ann) $
                        carAnnCaption ann)
            # rotate (Deg $ carAnnAngle ann)
            # translate (r2 $ carAnnPosition ann)
        }

instance LocatableAnnotation CarAnnotation where
    annPosition = carAnnPosition
    locateAnnotation pos ann = ann { carAnnPosition = pos }

instance OrientableAnnotation CarAnnotation where
    annAngle = carAnnAngle
    orientAnnotation ang ann = ann { carAnnAngle = ang }

data SegAnnotation
     = SegAnnotation
     { segAnnColour :: Colour Double
     , segAnnPosition :: (Double, Double)
     , segAnnAngle :: Double
     , segAnnLength :: Double
     , segAnnCaption :: CaptAnnotation
     } deriving (Show)

instance IsAnnotation SegAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            fromSegments
                [ straight (r2 (segAnnLength ann, 0))
                ]
            # stroke
            # lw 0.25 # lc (segAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . captAnnAlignment . segAnnCaption $ ann))
                (renderAnnotation . annotation
                    . rotateAnnotation (- segAnnAngle ann) $
                        segAnnCaption ann)
            # rotate (Deg $ segAnnAngle ann)
            # translate (r2 $ segAnnPosition ann)
        }

instance LocatableAnnotation SegAnnotation where
    annPosition = segAnnPosition
    locateAnnotation pos ann = ann { segAnnPosition = pos }

instance OrientableAnnotation SegAnnotation where
    annAngle = segAnnAngle
    orientAnnotation ang ann = ann { segAnnAngle = ang }

data SplitAnnotation
     = SplitAnnotation
     { splAnnColour :: Colour Double
     , splAnnPosition :: (Int, Int)
     , splAnnDirection :: CardinalDirection
     , splAnnLength :: Int
     , splAnnIndex :: Int
     , splAnnCaptBgOpacity :: Double
     , splAnnCaptAlignment :: CardinalDirection
     } deriving (Show)

instance IsAnnotation SplitAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            let (posX, posY) = splAnnPosition ann
                pos = (fromIntegral posX, fromIntegral posY)
            in fromSegments
                [ straight (r2 (fromIntegral $ splAnnLength ann, 0)
                # rotate (Deg $ cardinalDirToAngle $ splAnnDirection ann)) ]
            # stroke
            # lw 0.25 # lc (splAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . splAnnCaptAlignment $ ann))
                (renderAnnotation . annotation $
                    CaptAnnotation
                        { captAnnPosition = (0, 0)
                        , captAnnColour = splAnnColour ann
                        , captAnnBgOpacity = splAnnCaptBgOpacity ann
                        , captAnnAlignment = splAnnCaptAlignment ann
                        , captAnnAngle = 0
                        , captAnnSize = 0.75
                        , captAnnText = show $ splAnnIndex ann
                        }
                    )
            # translate (r2 pos)
        }

data CaptAnnotation = CaptAnnotation
    { captAnnPosition :: (Double, Double)
    , captAnnColour :: Colour Double
    , captAnnBgOpacity :: Double
    , captAnnAlignment :: CardinalDirection
    , captAnnAngle :: Double
    , captAnnSize :: Double
    , captAnnText :: String
    } deriving (Show)

instance IsAnnotation CaptAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            let dirAlign = - cardinalDirToR2 (captAnnAlignment ann)
                adjustAlignments (x, y) = ((x + 1) / 2, (y + 1) / 2)
                (xAlign, yAlign) = adjustAlignments $ unr2 dirAlign
            in (
                --alignedText xAlign yAlign caption
                text (captAnnText ann)
                # fc (captAnnColour ann) # applyStyle captionStyle
                <> uncurry rect (textBounds $ captAnnText ann)
                # fcA (computeBgColour (captAnnColour ann)
                    `withOpacity` (captAnnBgOpacity ann))
                # lw 0
            )
            # align dirAlign # scale (captAnnSize ann)
            # rotate (Deg $ captAnnAngle ann)
        }
        where
        captionStyle = mempty # bold
        -- The font metric corrections were defined by trial-and-error.
        extentsToBounds (fe, te) =
            let (_, h) = unr2 $ CairoText.textSize te
                (xa, _) = unr2 $ CairoText.advance te
                fh = CairoText.height fe
            in ((xa + h) / (0.8 * fh), 2 * h / fh)
        textBounds = extentsToBounds
            . CairoText.unsafeCairo . CairoText.getExtents captionStyle

instance LocatableAnnotation CaptAnnotation where
    annPosition = captAnnPosition
    locateAnnotation pos ann = ann { captAnnPosition = pos }

instance OrientableAnnotation CaptAnnotation where
    annAngle = captAnnAngle
    orientAnnotation ang ann = ann { captAnnAngle = ang }

cardinalDirToR2 :: CardinalDirection -> R2
cardinalDirToR2 x = case x of
    E -> unitX
    N -> unitY
    W -> unit_X
    S -> unit_Y

cardinalDirToAngle :: CardinalDirection -> Double
cardinalDirToAngle x = case x of
    E -> 0
    N -> 90
    W -> 180
    S -> 270

computeBgColour :: Colour Double -> Colour Double
computeBgColour c = sRGB r' g' b'
    where
    cRgb = toSRGB c
    (h, s, v) = hsvView cRgb
    h' = if h > 180 then h - 180 else h + 180
    (s', v') = monochromeSV (s, v)
    cHsv' = hsv h' s' v'
    r' = channelRed cHsv'
    g' = channelGreen cHsv'
    b' = channelBlue cHsv'

monochromeSV, polychromeSV, limitedPolychromeSV :: (Double, Double)
                                                -> (Double, Double)
monochromeSV (s, v) =
    if v > 0.65 || (v > 0.55 && s > 0.25)
       then (0, 0)
       else (0, 1)

polychromeSV = const (1, 1)

limitedPolychromeSV sv@(s, _) =
    if s > 0.25
        then polychromeSV sv
        else monochromeSV sv

{-
 - Old code kept around for reference in future refactorings.
 -

renderCaption colour captBgOpacity captAlign captAngle captSize caption =
    let dirAlign = - cardinalDirToR2 captAlign
        adjustAlignments (x, y) = ((x + 1) / 2, (y + 1) / 2)
        (xAlign, yAlign) = adjustAlignments $ unr2 dirAlign
    in (
        --alignedText xAlign yAlign caption
        text caption
        # fc colour # applyStyle captionStyle
        <> uncurry rect (textBounds caption)
        # fcA (computeBgColour colour `withOpacity` captBgOpacity) # lw 0
    )
    # align dirAlign # scale captSize # rotate (Deg captAngle)
    where
    captionStyle = mempty # bold
    -- The font metric corrections were defined by trial-and-error.
    extentsToBounds (fe, te) =
        let (_, h) = unr2 $ CairoText.textSize te
            (xa, _) = unr2 $ CairoText.advance te
            fh = CairoText.height fe
        in ((xa + h) / (0.8 * fh), 2 * h / fh)
    textBounds = extentsToBounds
        . CairoText.unsafeCairo . CairoText.getExtents captionStyle


-- Ideally, we would implement Split in terms of Seg. That, however, is
-- deferred until a satisfactory way of specifying the differences in how
-- alignments are specified in one case and the other is defined.
instance IsAnnotation SplitAnnotation where
    annotation ann = annotation $ SegAnnotation
        { segAnnColour = splAnnColour ann
        , segAnnPosition =
              let (x, y) = splAnnPosition ann
              in (fromIntegral x, fromIntegral y)
        , segAnnAngle = cardinalDirToAngle $ splAnnDirection ann
        , segAnnLength = fromIntegral $ splAnnLength ann
        , segAnnCaption = show $ splAnnIndex ann
        , segAnnCaptColour = splAnnColour ann
        , segAnnCaptAlignment = splAnnCaptAlignment ann
        , segAnnCaptAngle = 0
        , segAnnCaptSize = 0.75
        }
 -}

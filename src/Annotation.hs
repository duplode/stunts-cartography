{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Annotation
    ( CardinalDirection(..)
    , Annotation(..)
    , IsAnnotation (annotation)
    , renderAnnotation
    , defAnn
    , LocatableAnnotation
        ( annPosition
        , locateAnnotation
        , translateAnnotation
        )
    , OrientableAnnotation
        ( annAngle
        , orientAnnotation
        , rotateAnnotation
        )
    , ResizableAnnotation
        ( annSize
        , resizeAnnotation
        , scaleAnnotation
        )
    , ColourAnnotation
        ( annColour
        , setAnnColour
        , annColourIsProtected
        , protectAnnColour
        , customiseAnnColour
        , overrideAnnColour
        , deepOverrideAnnColour
        )
    , maybeCustomiseAnnColour
    , maybeDeepOverrideAnnColour
    , CarAnnotation
    , carAnnColour
    , carAnnOpacity
    , carAnnPosition
    , carAnnAngle
    , carAnnSize
    , carAnnCaption
    , SegAnnotation(..)
    , SplitAnnotation(..)
    , CaptAnnotation
    , captAnnPosition
    , captAnnColour
    , captAnnBgOpacity
    , captAnnAlignment
    , captAnnAngle
    , captAnnSize
    , captAnnText
    ) where

-- It might be sensible to import this qualified if you need the constructors.

import Control.Lens.Operators hiding ((#))
import qualified Control.Lens as L
import Data.Default
import Diagrams.Prelude
import qualified Diagrams.Backend.Cairo.Text as CairoText
import Control.Lens ((^.))
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV
import Types.Diagrams (BEDia)
import Pics.MM

data CardinalDirection = E
                       | N
                       | W
                       | S
                       deriving (Read, Show, Eq, Ord)

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

newtype Annotation
    = Annotation
    { annotationDiagram :: Diagram BEDia R2
    }

class IsAnnotation a where
    annotation :: a -> Annotation
    renderAnnotation :: a -> Diagram BEDia R2

    renderAnnotation = annotationDiagram . annotation

defAnn :: (Default a, IsAnnotation a) => a
defAnn = def

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

class (IsAnnotation a) => ResizableAnnotation a where
    annSize :: a -> Double
    resizeAnnotation :: Double -> a -> a
    scaleAnnotation :: Double -> a -> a

    scaleAnnotation qs ann = resizeAnnotation (qs * annSize ann) ann

-- Colour overriding for nested annotations.
-- Note that opacity is not handled, as it is not subject to overriding.
class (IsAnnotation a) => ColourAnnotation a where
    annColour :: a -> Colour Double
    setAnnColour :: Colour Double -> a -> a
    annColourIsProtected :: a -> Bool
    protectAnnColour :: a -> a
    customiseAnnColour :: Colour Double -> a -> a
    overrideAnnColour :: Colour Double -> a -> a
    deepOverrideAnnColour :: Colour Double -> a -> a

    customiseAnnColour cl = protectAnnColour . setAnnColour cl
    -- Using an override implemented in terms of customise means that no deep
    -- colour definition by the user can be overriden. That appears to be
    -- adequate for the typical use cases.
    overrideAnnColour cl ann =
        if not (annColourIsProtected ann)
            then customiseAnnColour cl ann
            else ann
    -- The following default implementation is meant to be overriden if the
    -- annotation contains other ColourAnnotations.
    deepOverrideAnnColour = overrideAnnColour

maybeCustomiseAnnColour :: (ColourAnnotation a)
                        => Maybe (Colour Double) -> a -> a
maybeCustomiseAnnColour = maybe id customiseAnnColour

maybeDeepOverrideAnnColour :: (ColourAnnotation a)
                           => Maybe (Colour Double) -> a -> a
maybeDeepOverrideAnnColour = maybe id deepOverrideAnnColour

data CaptAnnotation = CaptAnnotation
    { _captAnnPosition :: (Double, Double)
    , _captAnnColour :: Colour Double
    , _captAnnColourIsProtected :: Bool
    , _captAnnBgOpacity :: Double
    , _captAnnAlignment :: CardinalDirection
    , _captAnnAngle :: Double
    , _captAnnSize :: Double
    , _captAnnText :: String
    } deriving (Show)
L.makeLenses ''CaptAnnotation

instance Default CaptAnnotation where
    def = CaptAnnotation
        { _captAnnPosition = (0, 0)
        , _captAnnColour = yellow
        , _captAnnColourIsProtected = False
        , _captAnnBgOpacity = 0
        , _captAnnAlignment = E
        , _captAnnAngle = 0
        , _captAnnSize = 0.4
        , _captAnnText = ""
        }

instance IsAnnotation CaptAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            let dirAlign = - cardinalDirToR2 (_captAnnAlignment ann)
                adjustAlignments (x, y) = ((x + 1) / 2, (y + 1) / 2)
                (xAlign, yAlign) = adjustAlignments $ unr2 dirAlign
            in (
                --alignedText xAlign yAlign caption
                text (_captAnnText ann)
                # fc (_captAnnColour ann) # applyStyle captionStyle
                <> uncurry rect (textBounds $ _captAnnText ann)
                # fcA (computeBgColour (_captAnnColour ann)
                    `withOpacity` (_captAnnBgOpacity ann))
                # lw 0
            )
            # align dirAlign # scale (_captAnnSize ann)
            # rotate (Deg $ _captAnnAngle ann)
        }
        where
        captionStyle = mempty # bold
        -- The font metric corrections were defined by trial-and-error.
        extentsToBounds (fe, te) =
            let (_, h) = unr2 $ te ^. CairoText.textSize
                (xa, _) = unr2 $ te ^. CairoText.advance
                fh = fe ^. CairoText.height
            in ((xa + h) / (0.8 * fh), 2 * h / fh)
        textBounds = extentsToBounds
            . CairoText.unsafeCairo . CairoText.getExtents captionStyle

instance LocatableAnnotation CaptAnnotation where
    annPosition = _captAnnPosition
    locateAnnotation pos ann = ann & captAnnPosition .~ pos

instance OrientableAnnotation CaptAnnotation where
    annAngle = _captAnnAngle
    orientAnnotation ang ann = ann & captAnnAngle .~ ang

instance ColourAnnotation CaptAnnotation where
    annColour = _captAnnColour
    setAnnColour cl ann = ann & captAnnColour .~ cl
    annColourIsProtected = _captAnnColourIsProtected
    protectAnnColour ann = ann & captAnnColourIsProtected .~ True

data CarAnnotation
     = CarAnnotation
     { _carAnnColour :: Colour Double
     , _carAnnOpacity :: Double
     , _carAnnColourIsProtected :: Bool
     , _carAnnPosition :: (Double, Double)
     , _carAnnAngle :: Double
     , _carAnnSize :: Double
     , _carAnnCaption :: CaptAnnotation
     } deriving (Show)
L.makeLenses ''CarAnnotation

instance Default CarAnnotation where
    def = CarAnnotation
        { _carAnnColour = yellow
        , _carAnnOpacity = 1
        , _carAnnColourIsProtected = False
        , _carAnnPosition = (0, 0)
        , _carAnnAngle = 0
        , _carAnnSize = 0.5
        , _carAnnCaption = defAnn
        }

instance IsAnnotation CarAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            acura' (_carAnnColour ann) 1
            # opacity (_carAnnOpacity ann)
            # scale (_carAnnSize ann)
            # (flip $ beside
                (cardinalDirToR2 . _captAnnAlignment . _carAnnCaption $ ann))
                (renderAnnotation . rotateAnnotation (- _carAnnAngle ann) $
                    _carAnnCaption ann)
            # rotate (Deg $ _carAnnAngle ann)
            # translate (r2 $ _carAnnPosition ann)
        }

instance LocatableAnnotation CarAnnotation where
    annPosition = _carAnnPosition
    locateAnnotation pos ann = ann & carAnnPosition .~ pos

instance OrientableAnnotation CarAnnotation where
    annAngle = _carAnnAngle
    orientAnnotation ang ann = ann & carAnnAngle .~ ang

instance ResizableAnnotation CarAnnotation where
    annSize = _carAnnSize
    resizeAnnotation sz ann = ann & carAnnSize .~ sz

instance ColourAnnotation CarAnnotation where
    annColour = _carAnnColour
    setAnnColour cl ann = ann & carAnnColour .~ cl
    annColourIsProtected = _carAnnColourIsProtected
    protectAnnColour ann = ann & carAnnColourIsProtected .~ True
    deepOverrideAnnColour cl ann = overrideAnnColour cl $
        L.over carAnnCaption (deepOverrideAnnColour cl) ann

data SegAnnotation
     = SegAnnotation
     { segAnnColour :: Colour Double
     , segAnnColourIsProtected :: Bool
     , segAnnPosition :: (Double, Double)
     , segAnnAngle :: Double
     , segAnnLength :: Double
     , segAnnCaption :: CaptAnnotation
     } deriving (Show)

instance Default SegAnnotation where
    def = SegAnnotation
        { segAnnColour = yellow
        , segAnnColourIsProtected = False
        , segAnnPosition = (0, 0)
        , segAnnAngle = 0
        , segAnnLength = 1
        , segAnnCaption = defAnn
        }

instance IsAnnotation SegAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            fromSegments
                [ straight (r2 (segAnnLength ann, 0))
                ]
            # stroke
            # lw 0.25 # lc (segAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . _captAnnAlignment . segAnnCaption $ ann))
                (renderAnnotation . rotateAnnotation (- segAnnAngle ann) $
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

instance ColourAnnotation SegAnnotation where
    annColour = segAnnColour
    setAnnColour cl ann = ann { segAnnColour = cl }
    annColourIsProtected = segAnnColourIsProtected
    protectAnnColour ann = ann { segAnnColourIsProtected = True }
    deepOverrideAnnColour cl ann = overrideAnnColour cl
        ann { segAnnCaption = deepOverrideAnnColour cl $ segAnnCaption ann }

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
        { annotationDiagram =
            let (posX, posY) = splAnnPosition ann
                pos = (fromIntegral posX, fromIntegral posY)
            in fromSegments
                [ straight (r2 (fromIntegral $ splAnnLength ann, 0)
                # rotate (Deg $ cardinalDirToAngle $ splAnnDirection ann)) ]
            # stroke
            # lw 0.25 # lc (splAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . splAnnCaptAlignment $ ann))
                (renderAnnotation $
                    defAnn
                        { _captAnnColour = splAnnColour ann
                        , _captAnnBgOpacity = splAnnCaptBgOpacity ann
                        , _captAnnAlignment = splAnnCaptAlignment ann
                        , _captAnnSize = 0.75
                        , _captAnnText = show $ splAnnIndex ann
                        }
                    )
            # translate (r2 pos)
        }

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

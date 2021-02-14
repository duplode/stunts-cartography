{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Annotation
    ( CardinalDirection(..)
    , CarSprite(..)
    , Annotation(..)
    , IsAnnotation (annotation)
    , renderAnnotation
    , defAnn
    , LocatableAnnotation
        ( annPosition
        , translateAnnotation
        )
    , OrientableAnnotation
        ( annAngle
        , rotateAnnotation
        )
    , ResizableAnnotation
        ( annSize
        , scaleAnnotation
        )
    , ColourAnnotation
        ( annColour
        , annColourIsProtected
        , protectAnnColour
        , customiseAnnColour
        , overrideAnnColour
        , deepOverrideAnnColour
        )
    , TextAnnotation
        ( annText
        )
    , maybeCustomiseAnnColour
    , maybeDeepOverrideAnnColour

    , CarAnnotation
    , carAnnColour
    , carAnnOpacity
    , carAnnPosition
    , carAnnAngle
    , carAnnSize
    , carAnnLineWidth
    , carAnnInvert
    , carAnnCaption
    , carAnnSprite

    , SegAnnotation
    , segAnnColour
    , segAnnPosition
    , segAnnAngle
    , segAnnLength
    , segAnnWidth
    , segAnnCaption

    , SplitAnnotation
    , splAnnColour
    , splAnnPosition
    , splAnnDirection
    , splAnnLength
    , splAnnWidth
    , splAnnIndex
    , splAnnCaptBgOpacity
    , splAnnCaptAlignment
    , splAnnCaptInvert

    , CaptAnnotation
    , captAnnPosition
    , captAnnColour
    , captAnnBgOpacity
    , captAnnAlignment
    , captAnnAngle
    , captAnnInvert
    , captAnnSize
    , captAnnText
    ) where

import Data.Maybe (fromMaybe)

import Diagrams.Prelude hiding (E)
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV
import Graphics.SVGFonts (textSVG', TextOpts(..))

import qualified Util.SVGFonts as Util (bit)
import Util.Diagrams.Backend (BEDia)
import Pics.MM

data CardinalDirection = E
                       | N
                       | W
                       | S
                       deriving (Read, Show, Eq, Ord)

cardinalDirToR2 :: CardinalDirection -> V2 Double
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
    { annotationDiagram :: Diagram BEDia
    }

class IsAnnotation a where
    annotation :: a -> Annotation
    renderAnnotation :: a -> Diagram BEDia

    renderAnnotation = annotationDiagram . annotation

defAnn :: (Default a, IsAnnotation a) => a
defAnn = def

class (IsAnnotation a) => LocatableAnnotation a where
    annPosition :: Lens' a (Double, Double)  -- a -> (Double, Double)
    translateAnnotation :: (Double, Double) -> a -> a

    translateAnnotation (dx, dy) =
        annPosition %~ (\(xi, yi) -> (xi + dx, yi + dy))

-- Angles are assumed to be in degrees, for simplicity.
class (IsAnnotation a) => OrientableAnnotation a where
    annAngle :: Lens' a Double
    rotateAnnotation :: Double -> a -> a

    rotateAnnotation da = annAngle %~ (da +)

class (IsAnnotation a) => ResizableAnnotation a where
    annSize :: Lens' a Double
    scaleAnnotation :: Double -> a -> a

    scaleAnnotation qs = annSize %~ (qs *)

-- Colour overriding for nested annotations.
-- Note that opacity is not handled, as it is not subject to overriding.
class (IsAnnotation a) => ColourAnnotation a where
    annColour :: Lens' a (Colour Double)
    annColourIsProtected :: a -> Bool
    protectAnnColour :: a -> a
    customiseAnnColour :: Colour Double -> a -> a
    overrideAnnColour :: Colour Double -> a -> a
    deepOverrideAnnColour :: Colour Double -> a -> a

    customiseAnnColour cl = protectAnnColour . (annColour .~ cl)
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

class IsAnnotation a => TextAnnotation a where
    annText :: Lens' a String

data CaptAnnotation = CaptAnnotation
    { _captAnnPosition :: (Double, Double)
    , _captAnnColour :: Colour Double
    , _captAnnColourIsProtected :: Bool
    , _captAnnBgOpacity :: Double
    , _captAnnAlignment :: CardinalDirection
    , _captAnnAngle :: Double
    , _captAnnInvert :: Bool
    , _captAnnSize :: Double
    , _captAnnText :: String
    } deriving (Show)
makeLenses ''CaptAnnotation

instance Default CaptAnnotation where
    def = CaptAnnotation
        { _captAnnPosition = (0, 0)
        , _captAnnColour = yellow
        , _captAnnColourIsProtected = False
        , _captAnnBgOpacity = 0
        , _captAnnAlignment = E
        , _captAnnAngle = 0
        , _captAnnInvert = False
        , _captAnnSize = 0.4
        , _captAnnText = ""
        }

instance IsAnnotation CaptAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            let dirAlign = - cardinalDirToR2 (_captAnnAlignment ann)
                captText = textSVG' with
                        { textFont = Util.bit
                        , textHeight = _captAnnSize ann
                        } (_captAnnText ann)
                    # stroke # fillRule EvenOdd
                    # fc (_captAnnColour ann) # lwG 0
                initialBounding = boundingRect captText
                preframeFactor d z =
                    let z' = z + 2*d
                        q = z' / z
                    in if (z < 1e-4 || abs q < 1e-4)
                        -- Failing silently is acceptable for our current
                        -- purposes. A general-purpose implementation probably
                        -- should use Maybe.
                        then 1
                        else q
                preframeDelta = (max 0.04 . min 0.15) (0.3 * _captAnnSize ann)
                preframeFactor' =
                    preframeFactor preframeDelta . uncurry subtract
                (pfx, pfy) = maybe (1, 1)
                        -- The conditions for nothingess are the same for
                        -- extentX and extentY.
                        (\(x, y) -> (preframeFactor' x, preframeFactor' y))
                        ((,)
                            <$> extentX initialBounding
                            <*> extentY initialBounding)
            in (
                captText
                <> ( initialBounding
                    # scaleX pfx # scaleY pfy # frame (0.25 - preframeDelta)
                    # lwG 0
                    # fcA (computeBgColour (_captAnnColour ann)
                        `withOpacity` (_captAnnBgOpacity ann))
                    )
            )
            # center # (if _captAnnInvert ann then reflectX . reflectY else id)
            # align dirAlign
            # rotate (_captAnnAngle ann @@ deg)
            -- The translation should only matter for standalone captions.
            # translate (r2 $ _captAnnPosition ann)
        }

instance LocatableAnnotation CaptAnnotation where
    annPosition = captAnnPosition

instance OrientableAnnotation CaptAnnotation where
    annAngle = captAnnAngle

instance ColourAnnotation CaptAnnotation where
    annColour = captAnnColour
    annColourIsProtected = _captAnnColourIsProtected
    protectAnnColour = captAnnColourIsProtected .~ True

instance TextAnnotation CaptAnnotation where
    annText = captAnnText

data CarSprite
    = Acura
    | XMarker
    | CircleMarker
    | DiamondMarker
    | DotMarker
    | ArrowMarker
    deriving (Eq, Show, Enum)

spriteDiagram
    :: CarSprite      -- ^ Which sprite to use.
    -> Colour Double  -- ^ Main colour.
    -> Double         -- ^ Size.
    -> Maybe Double   -- ^ Line width. If 'Nothing', the value is
                      -- determined by the sprite implementation.
    -> Diagram BEDia
spriteDiagram spr = case spr of
    Acura -> acuraMarker
    XMarker -> xMarker
    CircleMarker -> circleMarker
    DiamondMarker -> diamondMarker
    DotMarker -> dotMarker
    ArrowMarker -> arrowMarker

-- CarAnnotation isn't just for cars, as the base sprite can be changed
-- while keeping the same functionality. It might be a good idea to
-- rename the type accordingly.
data CarAnnotation
     = CarAnnotation
     { _carAnnColour :: Colour Double
     , _carAnnOpacity :: Double
     , _carAnnColourIsProtected :: Bool
     , _carAnnPosition :: (Double, Double)
     , _carAnnAngle :: Double
     , _carAnnSize :: Double
     , _carAnnLineWidth :: Maybe Double  -- For arrow, x and circle.
     , _carAnnInvert :: Bool  -- For arrow markers.
     , _carAnnCaption :: CaptAnnotation
     , _carAnnSprite :: CarSprite
     } deriving (Show)
makeLenses ''CarAnnotation

instance Default CarAnnotation where
    def = CarAnnotation
        { _carAnnColour = yellow
        , _carAnnOpacity = 1
        , _carAnnColourIsProtected = False
        , _carAnnPosition = (0, 0)
        , _carAnnAngle = 0
        , _carAnnSize = 0.5
        , _carAnnLineWidth = Nothing
        , _carAnnInvert = False
        , _carAnnCaption = defAnn
        , _carAnnSprite = Acura
        }

instance IsAnnotation CarAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            spriteDiagram (_carAnnSprite ann)
                (_carAnnColour ann) (_carAnnSize ann) (_carAnnLineWidth ann)
            # opacity (_carAnnOpacity ann)
            # (case _carAnnSprite ann of
                ArrowMarker -> if _carAnnInvert ann
                    then alignL . reflectX . reflectY . center
                    else id
                _ -> id)
            # (flip $ beside
                (cardinalDirToR2 . _captAnnAlignment . _carAnnCaption $ ann))
                (renderAnnotation . rotateAnnotation (- _carAnnAngle ann) $
                    _carAnnCaption ann)
            # rotate (_carAnnAngle ann @@ deg)
            # translate (r2 $ _carAnnPosition ann)
        }

instance LocatableAnnotation CarAnnotation where
    annPosition = carAnnPosition

instance OrientableAnnotation CarAnnotation where
    annAngle = carAnnAngle

instance ResizableAnnotation CarAnnotation where
    annSize = carAnnSize

instance ColourAnnotation CarAnnotation where
    annColour = carAnnColour
    annColourIsProtected = _carAnnColourIsProtected
    protectAnnColour = carAnnColourIsProtected .~ True
    deepOverrideAnnColour cl = overrideAnnColour cl
        . (carAnnCaption %~ deepOverrideAnnColour cl)

instance TextAnnotation CarAnnotation where
    annText = carAnnCaption . captAnnText

data SegAnnotation
     = SegAnnotation
     { _segAnnColour :: Colour Double
     , _segAnnColourIsProtected :: Bool
     , _segAnnPosition :: (Double, Double)
     , _segAnnAngle :: Double
     , _segAnnLength :: Double
     , _segAnnWidth :: Maybe Double
     , _segAnnCaption :: CaptAnnotation
     } deriving (Show)

instance Default SegAnnotation where
    def = SegAnnotation
        { _segAnnColour = yellow
        , _segAnnColourIsProtected = False
        , _segAnnPosition = (0, 0)
        , _segAnnAngle = 0
        , _segAnnLength = 1
        , _segAnnWidth = Nothing
        , _segAnnCaption = defAnn
        }
makeLenses ''SegAnnotation

instance IsAnnotation SegAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            let centerDelta = angleV (_segAnnAngle ann @@ deg)
                    # scale (_segAnnLength ann / 2)
            in fromSegments
                [ straight (r2 (_segAnnLength ann, 0))
                ]
            # centerXY
            # strokePath
            # lwG (fromMaybe (min (1/4) (_segAnnLength ann / 4)) (_segAnnWidth ann))
            # lc (_segAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . _captAnnAlignment . _segAnnCaption $ ann))
                (renderAnnotation . rotateAnnotation (- _segAnnAngle ann) $
                    _segAnnCaption ann)
            # rotate (_segAnnAngle ann @@ deg)
            # translate (r2 (_segAnnPosition ann) + centerDelta)
        }

instance LocatableAnnotation SegAnnotation where
    annPosition = segAnnPosition

instance OrientableAnnotation SegAnnotation where
    annAngle = segAnnAngle

instance ColourAnnotation SegAnnotation where
    annColour = segAnnColour
    annColourIsProtected = _segAnnColourIsProtected
    protectAnnColour = segAnnColourIsProtected .~ True
    deepOverrideAnnColour cl = overrideAnnColour cl
        . (segAnnCaption %~ deepOverrideAnnColour cl)

instance TextAnnotation SegAnnotation where
    annText = segAnnCaption . captAnnText

data SplitAnnotation
     = SplitAnnotation
     { _splAnnColour :: Colour Double
     , _splAnnPosition :: (Int, Int)
     , _splAnnDirection :: CardinalDirection
     , _splAnnLength :: Int
     , _splAnnWidth :: Double
     , _splAnnIndex :: Int
     , _splAnnCaptBgOpacity :: Double
     , _splAnnCaptAlignment :: CardinalDirection
     , _splAnnCaptInvert :: Bool
     } deriving (Show)
makeLenses ''SplitAnnotation

instance Default SplitAnnotation where
    def = SplitAnnotation
        { _splAnnColour = yellow
        , _splAnnPosition = (1, 1)
        , _splAnnDirection = N
        , _splAnnLength = 1
        , _splAnnWidth = 0.25
        , _splAnnIndex = 1
        , _splAnnCaptBgOpacity = 1
        , _splAnnCaptAlignment = N
        , _splAnnCaptInvert = False
        }

instance IsAnnotation SplitAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            let (posX, posY) = _splAnnPosition ann
                pos = (fromIntegral posX, fromIntegral posY)
                centerDelta = cardinalDirToR2 (_splAnnDirection ann)
                    # scale (fromIntegral (_splAnnLength ann) / 2)
            in fromSegments
                [ straight (r2 (fromIntegral $ _splAnnLength ann, 0)
                # rotate (cardinalDirToAngle (_splAnnDirection ann) @@ deg)) ]
            # centerXY
            # strokePath
            # lwG (_splAnnWidth ann) # lc (_splAnnColour ann)
            # (flip $ beside
                (cardinalDirToR2 . _splAnnCaptAlignment $ ann))
                (renderAnnotation $ defAnn
                    & captAnnColour .~ _splAnnColour ann
                    & captAnnBgOpacity .~ _splAnnCaptBgOpacity ann
                    & captAnnAlignment .~ _splAnnCaptAlignment ann
                    & captAnnInvert .~ _splAnnCaptInvert ann
                    & captAnnSize .~ 0.75
                    & captAnnText .~ show (_splAnnIndex ann)
                    )
            # translate (r2 pos + centerDelta)
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
        # fcA (computeBgColour colour `withOpacity` captBgOpacity) # lwG 0
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

-- This is the IsAnnotation instance for CaptAnnotation before the move
-- to SVGFonts.
{-
instance IsAnnotation CaptAnnotation where
    annotation ann = Annotation
        { annotationDiagram =
            let dirAlign = - cardinalDirToR2 (_captAnnAlignment ann)
            in (
                text (_captAnnText ann)
                # fc (_captAnnColour ann) # applyStyle captionStyle
                -- Caption background.
                -- Disabled until a replacement for Cairo.Text is set up.
                {-
                <> uncurry rect
                    (CairoText.textBounds captionStyle $ _captAnnText ann)
                # fcA (computeBgColour (_captAnnColour ann)
                    `withOpacity` (_captAnnBgOpacity ann))
                # lwG 0
                -}
            )
            # alignBL # align dirAlign
            # scale (_captAnnSize ann)
            # rotate (_captAnnAngle ann @@ deg)
        }
        where
        captionStyle = mempty # bold
-}

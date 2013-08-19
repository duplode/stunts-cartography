{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Annotate where

-- It seems sensible to import this qualified if you need the raw constructors.

import Diagrams.Prelude
import Types (BEDia)
import MM

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

data CarAnnotation
     = CarAnnotation
     { carAnnColour :: Colour Double
     , carAnnPosition :: (Double, Double)
     , carAnnAngle :: Double
     , carAnnSize :: Double
     , carAnnCaption :: String
     , carAnnCaptColour :: Colour Double
     , carAnnCaptAlignment :: CardinalDirection
     , carAnnCaptAngle :: Double
     , carAnnCaptSize :: Double
     } deriving (Show)

data SegAnnotation
     = SegAnnotation
     { segAnnColour :: Colour Double
     , segAnnPosition :: (Double, Double)
     , segAnnAngle :: Double
     , segAnnLength :: Double
     , segAnnCaption :: String
     , segAnnCaptColour :: Colour Double
     , segAnnCaptAlignment :: CardinalDirection
     , segAnnCaptAngle :: Double
     , segAnnCaptSize :: Double
     } deriving (Show)

data SplitAnnotation
     = SplitAnnotation
     { splAnnColour :: Colour Double
     , splAnnPosition :: (Int, Int)
     , splAnnDirection :: CardinalDirection
     , splAnnLength :: Int
     , splAnnIndex :: Int
     , splAnnCaptAlignment :: CardinalDirection
     } deriving (Show)

instance IsAnnotation CarAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            acura' (carAnnColour ann) 1
            # rotate (Deg $ carAnnAngle ann)
            # (flip $ beside (cardinalDirToR2 $ carAnnCaptAlignment ann))
                (renderCaption
                    (carAnnCaptColour ann) (carAnnCaptAlignment ann)
                    (carAnnCaptAngle ann) (carAnnCaptSize ann)
                    (carAnnCaption ann))
            # scale (carAnnSize ann)
            # translate (r2 $ carAnnPosition ann)
        }

instance IsAnnotation SegAnnotation where
    annotation ann = Annotation
        { renderAnnotation =
            fromSegments
                [ straight (r2 (segAnnLength ann, 0)
                # rotate (Deg $ segAnnAngle ann)) ]
            # stroke
            # lw 0.25 # lc (segAnnColour ann)
            # (flip $ beside (cardinalDirToR2 $ segAnnCaptAlignment ann))
                (renderCaption
                    (segAnnCaptColour ann) (segAnnCaptAlignment ann)
                    (segAnnCaptAngle ann) (segAnnCaptSize ann)
                    (segAnnCaption ann))
            # translate (r2 $ segAnnPosition ann)
        }

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
        , segAnnCaptSize = 0.5
        }

renderCaption colour captAlign captAngle captSize caption =
    text caption # scale captSize # rotate (Deg captAngle)
    # fc colour
    -- TODO: Changing the rectangle size doesn't seem to change padding.
    <> rect captSize captSize # lw 0
    # align (cardinalDirToR2 captAlign)

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

module AnnotationTypes where

-- It seems sensible to import this qualified if you need the raw constructors.

import Diagrams.Prelude (Colour)

data CaptionAlignment = E
                      | N
                      | W
                      | S
                      deriving (Read, Show, Eq, Ord)

-- TODO: Do something to have better constructors.
-- Maybe add a wrapper typeclass, or play with existentials to hide the fields.
data Annotation = Car (Colour Double) (Double, Double) Double Double
                      String CaptionAlignment Double Double
                | Seg (Colour Double) (Double, Double) Double Double
                      String CaptionAlignment Double Double
                deriving (Read, Show)

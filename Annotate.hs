module Annotate
    ( Annotation
    , readAnnotationsMinimal
    , renderAnnotation
    ) where


import Diagrams.Prelude
import Track (Orientation(..))
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Pics (acura)
import AnnotationTypes

-- This will fail silently for now.
readAnnotationsMinimal :: String -> [Annotation]
readAnnotationsMinimal = catMaybes . map readMaybe . lines

-- renderAnnotation :: Annotation -> "Dia"
renderAnnotation ann = case ann of

    Car colour pos angle size
        caption captAlign captAngle captSize
        ->
        acura colour # scale size
        # rotate (Deg angle)
        # translate (r2 pos)
        -- TODO: Add caption

    Seg colour pos angle len
        caption captAlign captAngle captSize
        ->
        fromSegments [ straight (r2 (len, 0) # rotate (Deg angle)) ]
        # stroke
        # lw 0.25 # lc colour
        # translate (r2 pos)
        -- TODO: Add caption

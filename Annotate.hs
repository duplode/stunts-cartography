module Annotate
    ( Annotation
    , readAnnotationsMinimal
    , renderAnnotation
    ) where


import Diagrams.Prelude
import Track (Orientation(..))
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Pics (acura')
import AnnotationTypes

-- This will fail silently for now.
readAnnotationsMinimal :: String -> [Annotation]
readAnnotationsMinimal = catMaybes . map readMaybe . lines

-- renderAnnotation :: Annotation -> "Dia"
renderAnnotation ann = case ann of

    Car colour pos angle size
        caption captAlign captAngle captRelSize
        ->
        acura' colour 1
        # rotate (Deg angle)
        # (flip $ beside (captAlignToDirection captAlign))
            (renderCaption colour captAlign captAngle captRelSize caption)
        # scale size
        # translate (r2 pos)

    Seg colour pos angle len
        caption captAlign captAngle captSize
        ->
        fromSegments [ straight (r2 (len, 0) # rotate (Deg angle)) ]
        # stroke
        # lw 0.25 # lc colour
        # (flip $ beside (captAlignToDirection captAlign))
            (renderCaption colour captAlign captAngle captSize caption)
        # translate (r2 pos)

renderCaption colour captAlign captAngle captSize caption =
    text caption # scale captSize # rotate (Deg captAngle)
    # fc colour
    -- TODO: Changing the rectangle size doesn't seem to change padding.
    <> rect captSize captSize # lw 0
    # align (captAlignToDirection captAlign)

captAlignToDirection :: CaptionAlignment -> R2
captAlignToDirection x = case x of
    E -> unitX
    N -> unitY
    W -> unit_X
    S -> unit_Y

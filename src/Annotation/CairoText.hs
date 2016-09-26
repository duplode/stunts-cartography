module Annotation.CairoText
    ( textBounds
    ) where

import Control.Lens ((^.))
import Diagrams.Prelude (Style, R2, unr2)
import qualified Diagrams.Backend.Cairo.Text as CairoText

-- Isolating the Cairo.Text-based hacks in a separate module.
-- These functions were used along with diagrams-cairo 1.2 to provide
-- caption backgrounds. Currently there are better options -- pango
-- support in diagrams-cairo and the SVGFonts package.

-- The font metric corrections were defined by trial-and-error.
extentsToBounds :: (CairoText.FontExtents, CairoText.TextExtents)
                -> (Double, Double)
extentsToBounds (fe, te) =
    let (_, h) = unr2 $ te ^. CairoText.textSize
        (xa, _) = unr2 $ te ^. CairoText.advance
        fh = fe ^. CairoText.height
    in ((xa + h) / (0.8 * fh), 2 * h / fh)

textBounds :: Style R2 -> String -> (Double, Double)
textBounds style = extentsToBounds
    . CairoText.unsafeCairo . CairoText.getExtents style

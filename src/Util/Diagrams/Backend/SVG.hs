module Util.Diagrams.Backend.SVG
    ( BEDia
    , renderBE
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG (SVG, renderSVG)

type BEDia = SVG

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderSVG

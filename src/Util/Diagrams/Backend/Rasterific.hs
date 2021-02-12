module Util.Diagrams.Backend.Rasterific
    ( BEDia
    , renderBE
    ) where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)

type BEDia = Rasterific

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderRasterific

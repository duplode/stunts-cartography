module Util.Diagrams.Backend.Cairo
    ( BEDia
    , renderBE
    ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo (Cairo, renderCairo)

type BEDia = Cairo

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderCairo

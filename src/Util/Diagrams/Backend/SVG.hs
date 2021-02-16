module Util.Diagrams.Backend.SVG
    ( BEDia
    , renderBE
    , forkRender
    ) where

import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.SVG (SVG, renderSVG)

type BEDia = SVG

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderSVG

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

module Util.Diagrams.Backend.Rasterific
    ( BEDia
    , renderBE
    , forkRender
    ) where

import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)

type BEDia = Rasterific

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderRasterific

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

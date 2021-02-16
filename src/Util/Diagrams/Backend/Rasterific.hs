module Util.Diagrams.Backend.Rasterific
    ( BEDia
    , outputTypes
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)

import Util.Diagrams.Backend.Common (OutputType(..))

type BEDia = Rasterific

outputTypes :: NonEmpty OutputType
outputTypes = PNG :| []

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderRasterific

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

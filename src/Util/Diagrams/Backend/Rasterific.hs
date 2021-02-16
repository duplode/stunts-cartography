module Util.Diagrams.Backend.Rasterific
    ( B
    , outputTypes
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B, renderRasterific)

import Util.Diagrams.Backend.Common (OutputType(..))

outputTypes :: NonEmpty OutputType
outputTypes = PNG :| []

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderRasterific

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

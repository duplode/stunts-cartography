module Util.Diagrams.Backend.Rasterific
    ( B
    , availableOutputTypes
    , widthConversionFactor
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B, renderRasterific)

import Util.Diagrams.Backend.Common (OutputType(..))

availableOutputTypes :: NonEmpty OutputType
availableOutputTypes = PNG :| []

widthConversionFactor :: OutputType -> Double
widthConversionFactor = const 1

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderRasterific

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

module Util.Diagrams.Backend.SVG
    ( B
    , availableOutputTypes
    , widthConversionFactor
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.SVG (B, renderSVG)

import Util.Diagrams.Backend.Common (OutputType(..))

availableOutputTypes :: NonEmpty OutputType
availableOutputTypes = SVG :| []

widthConversionFactor :: OutputType -> Double
widthConversionFactor = const 1

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderSVG

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

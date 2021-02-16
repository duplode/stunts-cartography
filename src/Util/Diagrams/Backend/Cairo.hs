module Util.Diagrams.Backend.Cairo
    ( B
    , availableOutputTypes
    , widthConversionFactor
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkOS, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Cairo (B, renderCairo)

import Util.Diagrams.Backend.Common (OutputType(..))

availableOutputTypes :: NonEmpty OutputType
availableOutputTypes = PNG :| [SVG]

-- cairo uses px for rendering PNGs but pt for SVGs. The conversion
-- factor follows the CSS specification.
widthConversionFactor :: OutputType -> Double
widthConversionFactor ot = case ot of
    PNG -> 1
    SVG -> 0.75

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderCairo

-- cairo uses thread-local state, which requires using forkOS rather
-- than forkIO.
forkRender :: IO () -> IO ThreadId
forkRender = forkOS

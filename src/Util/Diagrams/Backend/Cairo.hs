module Util.Diagrams.Backend.Cairo
    ( B
    , outputTypes
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkOS, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Cairo (B, renderCairo)

import Util.Diagrams.Backend.Common (OutputType(..))

outputTypes :: NonEmpty OutputType
outputTypes = PNG :| [SVG]

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderCairo

forkRender :: IO () -> IO ThreadId
forkRender = forkOS

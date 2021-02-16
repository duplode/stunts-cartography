module Util.Diagrams.Backend.SVG
    ( BEDia
    , outputTypes
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkIO, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.SVG (SVG, renderSVG)

import Util.Diagrams.Backend.Common (OutputType(..))

type BEDia = SVG

outputTypes :: NonEmpty OutputType
outputTypes = SVG :| []

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderSVG

forkRender :: IO () -> IO ThreadId
forkRender = forkIO

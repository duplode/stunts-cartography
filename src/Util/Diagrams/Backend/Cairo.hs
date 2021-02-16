module Util.Diagrams.Backend.Cairo
    ( BEDia
    , outputTypes
    , renderBE
    , forkRender
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Concurrent (forkOS, ThreadId)
import Diagrams.Prelude
import Diagrams.Backend.Cairo (Cairo, renderCairo)

import Util.Diagrams.Backend.Common (OutputType(..))

type BEDia = Cairo

outputTypes :: NonEmpty OutputType
outputTypes = PNG :| [SVG]

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram BEDia V2 Double Any -> IO ()
renderBE = renderCairo

forkRender :: IO () -> IO ThreadId
forkRender = forkOS

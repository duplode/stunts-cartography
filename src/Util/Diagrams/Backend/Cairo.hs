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

-- This is a vestigial function, to be removed once it becomes fully
-- clear it won't be needed in the future. It used to be the case that
-- cairo used px for rendering PNGs but pt for SVGs. That being so, a
-- conversion factor was introduced to make up the differemce. The
-- factor followed the CSS specification, being 1 for PNG and 0.75 for
-- SVG. It appears that recent versions of cairo do not require this
-- workaround (the most recent check was done with cairo-0.13.8.2 and
-- the repository head between that and cairo-0.13.9.0).
widthConversionFactor :: OutputType -> Double
widthConversionFactor _ = 1

renderBE :: FilePath -> SizeSpec V2 Double -> QDiagram B V2 Double Any -> IO ()
renderBE = renderCairo

-- cairo uses thread-local state, which requires using forkOS rather
-- than forkIO.
forkRender :: IO () -> IO ThreadId
forkRender = forkOS

module Parameters where

import Diagrams.Backend.Cairo (OutputType(..))
import Track (Horizon(..))

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    , bridgeHeight :: Double
    , bridgeRelativeWidth :: Double
    , bankingRelativeHeight :: Double

    , pixelsPerTile :: Double
    , drawGridLines :: Bool
    , drawIndices :: Bool
    , outputType :: OutputType
    }

defaultRenderingParameters = RenderingParameters
    { roadWidth = 1 / 5
    , bridgeHeight = 0
    , bridgeRelativeWidth = 2
    , bankingRelativeHeight = 1 / 2
    , pixelsPerTile = 32
    , drawGridLines = True
    , drawIndices = True
    , outputType = PNG
    }

data PostRenderInfo = PostRenderInfo
    { renderedTrackHorizon :: Horizon
    }

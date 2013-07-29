module Parameters where

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    , bridgeHeight :: Double
    , bridgeRelativeWidth :: Double
    , bankingRelativeHeight :: Double

    , pixelsPerTile :: Double
    }

defaultRenderingParameters = RenderingParameters
    { roadWidth = 1 / 5
    , bridgeHeight = 0
    , bridgeRelativeWidth = 2
    , bankingRelativeHeight = 1 / 2
    , pixelsPerTile = 32
    }

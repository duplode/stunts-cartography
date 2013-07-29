module Parameters where

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    , bridgeHeight :: Double
    , bridgeRelativeWidth :: Double
    }

defaultRenderingParameters = RenderingParameters
    { roadWidth = 1 / 5
    , bridgeHeight = 0
    , bridgeRelativeWidth = 2
    }

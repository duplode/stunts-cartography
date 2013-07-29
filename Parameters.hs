module Parameters where

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    }

defaultRenderingParameters = RenderingParameters
    { roadWidth = 1 / 5
    }

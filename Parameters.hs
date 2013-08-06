module Parameters where

import Diagrams.Backend.Cairo (OutputType(..))
import Track (Horizon(..))
import qualified OurByteString as LB

-- Data types which shift information across the various layers of the
-- rendering programs. It is probably a good idea to import it qualified, for
-- the sake of clarity.

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
    , trackName :: String
    , trackData :: LB.ByteString
    , outputPath :: FilePath
    }

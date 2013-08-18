module Parameters where

import Diagrams.Backend.Cairo (OutputType(..))
import Track (Horizon(..))
import qualified OurByteString as LB
import Annotate (Annotation)

-- Data types which shift information across the various layers of the
-- rendering programs. It is probably a good idea to import it qualified, for
-- the sake of clarity.

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    , bridgeHeight :: Double
    , bridgeRelativeWidth :: Double
    , bankingRelativeHeight :: Double

    , drawGridLines :: Bool
    , drawIndices :: Bool
    , xTileBounds :: (Int, Int)
    , yTileBounds :: (Int, Int)

    , annotationSpecs :: [Annotation]

    , pixelsPerTile :: Double
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
    , xTileBounds = (0, 29)
    , yTileBounds = (0, 29)
    , annotationSpecs = []
    }

widerRoadsRenderingParameters = defaultRenderingParameters
    { roadWidth = 1 / 4
    }

slopingRampsRenderingParameters = defaultRenderingParameters
    { bridgeRelativeWidth = 3 / 2
    , bridgeHeight = 1 / 4
    }

classicRenderingParameters = defaultRenderingParameters
    { roadWidth = 1 / 3
    , bridgeHeight = 4 / 15
    , bridgeRelativeWidth = 7 / 5
    , bankingRelativeHeight = 2 / 5
    }

minTileBounds :: (Num a) => RenderingParameters -> (a, a)
minTileBounds params =
    (fromIntegral . fst $ xTileBounds params, fromIntegral . fst $ yTileBounds params)

maxTileBounds :: (Num a) => RenderingParameters -> (a, a)
maxTileBounds params =
    (fromIntegral . snd $ xTileBounds params, fromIntegral . snd $ yTileBounds params)

-- The bounds are *tile* bounds. That means that a minBound of 0 and a maxBound
-- of 0 include *one* tile, the zeroth tile.
deltaTileBounds :: (Num a) => RenderingParameters -> (a, a)
deltaTileBounds params =
    let (xMin, xMax) = xTileBounds params
        (yMin, yMax) = yTileBounds params
    in (fromIntegral $ xMax - xMin + 1, fromIntegral $ yMax - yMin + 1)

data PostRenderInfo = PostRenderInfo
    { renderedTrackHorizon :: Horizon
    , trackName :: String
    , trackData :: LB.ByteString
    , outputPath :: FilePath
    }

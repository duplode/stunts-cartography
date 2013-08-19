module Parameters where

import Data.Map (Map)
import qualified Data.Map as M
import Diagrams.Prelude
import Diagrams.Backend.Cairo (OutputType(..))
import Track (Horizon(..), Element)
import qualified OurByteString as LB
import Annotate (Annotation)
import Types (BEDia)

-- Data types which shift information across the various layers of the
-- rendering programs. It is probably a good idea to import it qualified, for
-- the sake of clarity.

-- Static environment.

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

-- Extra output from the rendering.

data PostRenderInfo = PostRenderInfo
    { renderedTrackHorizon :: Horizon
    , trackName :: String
    , trackData :: LB.ByteString
    , outputPath :: FilePath
    }

-- Rendering state

data RenderingState = RenderingState
    { elementCache :: Map Element (Diagram BEDia R2)
    }

initialRenderingState :: RenderingState
initialRenderingState = RenderingState
    { elementCache = M.empty
    }

clearElementCache :: RenderingState -> RenderingState
clearElementCache st = st{ elementCache = M.empty }

insertIntoElementCache :: Element -> Diagram BEDia R2
                       -> RenderingState -> RenderingState
insertIntoElementCache el dia st = st{ elementCache = M.insert el dia $ elementCache st }

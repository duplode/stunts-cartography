{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Parameters
    ( RenderingParameters(..)
    , defaultRenderingParameters, widerRoadsRenderingParameters
    , slopingRampsRenderingParameters, classicRenderingParameters
    , minTileBounds, maxTileBounds, deltaTileBounds
    , RenderingElemStyle(..), toElemStyle
    , PostRenderInfo(..)
    , RenderingState(..), initialRenderingState
    , clearElementCache, clearTerrainCache
    , insertIntoElementCache, insertIntoTerrainCache
    , incrementNumberOfRuns
    , RenderingLog(..)
    , logToString, logFromString, logToText, logFromText
    ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Default.Class
import Diagrams.Prelude

import Track (Horizon(..), Element, Terrain)
import qualified Data.ByteString.Lazy as LB
import Annotation (Annotation)
--import Annotation.LapTrace (TraceAnnotation)
import Annotation.Flipbook
import Util.Diagrams.Backend

-- Data types which shift information across the various layers of the
-- rendering programs. It is probably a good idea to import it qualified, for
-- the sake of clarity.

-- Static environment.

data RenderingParameters = RenderingParameters
    { roadWidth :: Double
    , bridgeHeight :: Double
    , bridgeRelativeWidth :: Double
    , bankingRelativeHeight :: Double

    , twoToneTerrain :: Bool
    , transparentBg :: Bool

    , drawGridLines :: Bool
    , drawIndices :: Bool
    , xTileBounds :: (Int, Int)
    , yTileBounds :: (Int, Int)

    , annotationSpecs :: [Annotation]
    , flipbookSpec :: [SomeFlipbook]

    , pixelsPerTile :: Double
    , outputType :: OutputType

    , temporaryDirectory :: FilePath
    , baseDirectory :: FilePath
    }

defaultRenderingParameters = RenderingParameters
    { roadWidth = 1 / 5
    , bridgeHeight = 0
    , bridgeRelativeWidth = 2
    , bankingRelativeHeight = 1 / 2
    , twoToneTerrain = False
    , transparentBg = False
    , pixelsPerTile = 32 -- TODO: Slight misnomer.
    , drawGridLines = True
    , drawIndices = True
    , outputType = defaultOutputType
    , xTileBounds = (0, 29)
    , yTileBounds = (0, 29)
    , annotationSpecs = []
    , flipbookSpec = []
    , temporaryDirectory = "."
    , baseDirectory = "."
    }

instance Default RenderingParameters where
    def = defaultRenderingParameters

widerRoadsRenderingParameters = def
    { roadWidth = 1 / 4
    }

slopingRampsRenderingParameters = def
    { bridgeRelativeWidth = 3 / 2
    , bridgeHeight = 1 / 4
    }

classicRenderingParameters = def
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

-- Opaque type for tracking changes in the interface.
-- We might, in principle, separate element and terrain parameters; however,
-- performance gains are expected to be minimal.
newtype RenderingElemStyle = RenderingElemStyle (Double, Double, Double, Double, Bool)
    deriving (Eq)

toElemStyle :: RenderingParameters -> RenderingElemStyle
toElemStyle params = RenderingElemStyle
    ( roadWidth params, bridgeHeight params
    , bridgeRelativeWidth params, bankingRelativeHeight params
    , twoToneTerrain params)

-- Extra output from the rendering.

data PostRenderInfo = PostRenderInfo
    { renderedTrackHorizon :: Horizon
    , trackName :: String
    , trackData :: LB.ByteString
    , outputRelPath :: FilePath
    , flipbookRelPath :: Maybe FilePath
    }

-- Rendering state.

data RenderingState = RenderingState
    { elementCache :: Map Element (Diagram B)
    , terrainCache :: Map Terrain (Diagram B)
    , numberOfRuns :: Int
    }

initialRenderingState :: RenderingState
initialRenderingState = RenderingState
    { elementCache = M.empty
    , terrainCache = M.empty
    , numberOfRuns = 0
    }

instance Default RenderingState where
    def = initialRenderingState

clearElementCache :: RenderingState -> RenderingState
clearElementCache st = st{ elementCache = M.empty }

clearTerrainCache :: RenderingState -> RenderingState
clearTerrainCache st = st{ terrainCache = M.empty }

insertIntoElementCache :: Element -> Diagram B
                       -> RenderingState -> RenderingState
insertIntoElementCache el dia st = st{ elementCache = M.insert el dia $ elementCache st }

insertIntoTerrainCache :: Terrain -> Diagram B
                       -> RenderingState -> RenderingState
insertIntoTerrainCache te dia st = st{ terrainCache = M.insert te dia $ terrainCache st }

incrementNumberOfRuns :: RenderingState -> RenderingState
incrementNumberOfRuns st = st{ numberOfRuns = numberOfRuns st + 1 }

-- Translucent writer type.

newtype RenderingLog = RenderingLog { unRenderingLog :: Text }
    deriving (Eq, Ord, Show, Semigroup, Monoid)

logFromString :: String -> RenderingLog
logFromString = RenderingLog . Text.pack

logToString :: RenderingLog -> String
logToString = Text.unpack . unRenderingLog

logFromText :: Text -> RenderingLog
logFromText = RenderingLog

logToText :: RenderingLog -> Text
logToText = unRenderingLog

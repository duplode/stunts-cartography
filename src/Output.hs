{-# LANGUAGE NoMonomorphismRestriction #-}
module Output
    ( writePngFromTrk
    , writePngFromRpl
    ) where

import Data.Array
import Control.Monad.Identity (runIdentity)
import Control.Monad.RWS hiding ((<>))
import Control.Exception (catch, SomeException)
import System.Directory (getTemporaryDirectory)
import System.FilePath (takeBaseName, (</>))
import System.CPUTime
import Text.Printf (printf)
import qualified OurByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Track (Tile, veryRawReadTrack, rawTrackToTileArray, horizonFromRawTrack)
import Utils
import Replay
import Composition
import qualified Parameters as Pm
import Annotate (renderAnnotation)
import CartoM
import Types

-- TODO: We probably should convert these write functions into CartoT IO.
writePngFromTrk :: FilePath -> CartoT IO Pm.PostRenderInfo
writePngFromTrk trkPath =
    liftIO (LB.readFile trkPath)
        >>= writePngOutput (takeBaseName trkPath)

writePngFromRpl :: FilePath -> CartoT IO Pm.PostRenderInfo
writePngFromRpl rplPath = do
    rplData <- liftIO $ LB.readFile rplPath
    let (trkName, trkData) = trkFromRplSimple rplData
    writePngOutput trkName trkData

writePngOutput :: String -> LB.ByteString -> CartoT IO Pm.PostRenderInfo
writePngOutput trackName trkBS = do
    params <- ask
    st <- get
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    let renWidthInTiles = (if Pm.drawIndices params then (2+) else id)
            . fst $ Pm.deltaTileBounds params
        renWidth = renWidthInTiles * Pm.pixelsPerTile params
        outType = Pm.outputType params
        outRelPath = case outType of
            PNG -> "stunts-cartography-map.png"
            SVG -> "stunts-cartography-map.svg"
            _   -> error "Unsupported output format."
    tmpDir <- liftIO $ getTemporaryDirectory
        `catch` ((\_ -> return ".") :: SomeException -> IO String)
    let outFile = tmpDir </> outRelPath

    startTime <- liftIO getCPUTime
    wholeMap <- wholeMapDiagram tiles
    liftIO . fst $ renderDia Cairo
        (CairoOptions outFile (Width renWidth) outType False) wholeMap
    endTime <- liftIO getCPUTime

    let fullDeltaTime = fromIntegral (endTime - startTime) / 10^9 :: Double
    tell . Pm.logFromList $
        printf "Rendering time (core + output writing): %0.0fms.\r\n" fullDeltaTime

    return Pm.PostRenderInfo
        { Pm.renderedTrackHorizon = horizon
        , Pm.trackName = trackName
        , Pm.trackData = trkBS
        , Pm.outputPath = outFile
        }

-- TODO: Possibly generalize the CartoM computations in Composition and below.
wholeMapDiagram :: (Monad m) => [Tile] -> CartoT m (Diagram BEDia R2)
wholeMapDiagram tiles = mapRWST (return . runIdentity) $ do
    params <- ask
    let minBounds = Pm.minTileBounds params
        (minX, minY) = minBounds
        deltaBounds = Pm.deltaTileBounds params
        (deltaX, deltaY) = deltaBounds
        adjustPositionForClip = moveOriginBy (r2 minBounds # reflectX # reflectY)
        clipRect :: Path R2
        clipRect = unitSquare # scaleX deltaX # scaleY deltaY
            # alignBL # adjustPositionForClip
    wholeMap <- renderMap tiles
    indices <- renderIndicesIfRequired
    return $
        (mconcat . map renderAnnotation $ Pm.annotationSpecs params)
        <>
        (indices # adjustPositionForClip)
        <>
        (
            (if Pm.drawGridLines params then gridLines else mempty)
            <>
            wholeMap
        )
        # clipBy clipRect
        # withEnvelope clipRect


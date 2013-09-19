{-# LANGUAGE NoMonomorphismRestriction #-}
module Output
    ( writePngFromTrk
    , writePngFromRpl
    ) where

import Data.Array
import Control.Monad.Identity (runIdentity)
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Error
import Control.Exception (catch, SomeException)
import System.FilePath (takeBaseName, (</>))
import System.CPUTime
import Text.Printf (printf)
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Track (Tile, veryRawReadTrack, rawTrackToTileArray, horizonFromRawTrack)
import qualified Util.ByteString as LB
import Util.Misc
import Replay
import Composition
import qualified Parameters as Pm
import Annotation (renderAnnotation)
import Types.CartoM
import Types.Diagrams

-- TODO: We probably should convert these write functions into CartoT IO.
writePngFromTrk :: FilePath -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writePngFromTrk trkPath =
    liftIO (LB.readFile trkPath)
        >>= writePngOutput (takeBaseName trkPath)

writePngFromRpl :: FilePath -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writePngFromRpl rplPath = do
    rplData <- liftIO $ LB.readFile rplPath
    if trackDataHasTheCorrectSize rplData
        then uncurry writePngOutput $ trkFromRplSimple rplData
        else lift $ throwError "No track data in RPL file."

writePngOutput :: String -> LB.ByteString
               -> CartoT (ErrorT String IO) Pm.PostRenderInfo
writePngOutput trackName trkBS = do
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr

    renWidthInTiles <- pure (\drawIx -> if drawIx then (2+) else id)
        <*> asks Pm.drawIndices <*> asks (fst . Pm.deltaTileBounds)
    renWidth <- pure (renWidthInTiles *) <*> asks Pm.pixelsPerTile
    tmpDir <- asks Pm.temporaryDirectory
    outType <- asks Pm.outputType
    let outRelPath = case outType of
            PNG -> "stunts-cartography-map.png"
            SVG -> "stunts-cartography-map.svg"
            _   -> error "Unsupported output format."
        outFile = tmpDir </> outRelPath

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


{-# LANGUAGE NoMonomorphismRestriction #-}
module Output
    ( writePngFromTrk
    , writePngFromRpl
    ) where

import Data.Array
import Control.Monad.RWS hiding ((<>))
import Control.Exception (catch, SomeException)
import System.Directory (getTemporaryDirectory)
import System.FilePath (takeBaseName, (</>))
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
writePngFromTrk :: Pm.RenderingParameters -> FilePath -> IO Pm.PostRenderInfo
writePngFromTrk params trkPath =
    LB.readFile trkPath >>= writePngOutput params (takeBaseName trkPath)

writePngFromRpl ::  Pm.RenderingParameters -> FilePath -> IO Pm.PostRenderInfo
writePngFromRpl params rplPath = do
    rplData <- LB.readFile rplPath
    let (trkName, trkData) = trkFromRplSimple rplData
    writePngOutput params trkName trkData

writePngOutput :: Pm.RenderingParameters -> String
               -> LB.ByteString -> IO Pm.PostRenderInfo
writePngOutput params trackName trkBS = do
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
    tmpDir <- getTemporaryDirectory
        `catch` ((\_ -> return ".") :: SomeException -> IO String)
    let outFile = tmpDir </> outRelPath

    let st = Pm.initialRenderingState -- TODO: Actually load it from somewhere.
        -- TODO: Actually use the extra data.
        (wholeMap,_,_) = runRWS (wholeMapDiagram tiles) params st
    fst . renderDia Cairo (CairoOptions outFile (Width renWidth) outType False) $
        wholeMap

    return Pm.PostRenderInfo
        { Pm.renderedTrackHorizon = horizon
        , Pm.trackName = trackName
        , Pm.trackData = trkBS
        , Pm.outputPath = outFile
        }

wholeMapDiagram :: [Tile] -> CartoM (Diagram BEDia R2)
wholeMapDiagram tiles = do
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

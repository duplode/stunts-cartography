{-# LANGUAGE NoMonomorphismRestriction #-}
module Output
    ( writePngFromTrk
    , writePngFromRpl
    ) where

import Data.Array
import Control.Monad.Trans.Reader
import Control.Exception (catch, SomeException)
import System.Directory (getTemporaryDirectory)
import System.FilePath (takeBaseName, (</>))
import qualified OurByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Track (veryRawReadTrack, rawTrackToTileArray, horizonFromRawTrack)
import Utils
import Replay
import LapTrace
import Composition
import qualified Parameters as Pm

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
    let willRenderIndices = Pm.drawIndices params
        renWidthInTiles = if willRenderIndices then 32 else 30
        renWidth = renWidthInTiles * Pm.pixelsPerTile params
        outType = Pm.outputType params
        outRelPath = case outType of
            PNG -> "stunts-cartography-map.png"
            SVG -> "stunts-cartography-map.svg"
            _   -> error "Unsupported output format."
    tmpDir <- getTemporaryDirectory
        `catch` ((\_ -> return ".") :: SomeException -> IO String)
    let outFile = tmpDir </> outRelPath

    fst . renderDia Cairo (CairoOptions outFile (Width renWidth) outType False) $
        --TODO: Restore the capability of tracing paths.
        (if Pm.drawGridLines params then gridLines else mempty)
        <>
        (if willRenderIndices then renderIndices else mempty)
        <>
        runReader (renderMap tiles) params

    return Pm.PostRenderInfo
        { Pm.renderedTrackHorizon = horizon
        , Pm.trackName = trackName
        , Pm.trackData = trkBS
        , Pm.outputPath = outFile
        }


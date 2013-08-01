{-# LANGUAGE NoMonomorphismRestriction #-}
module Output where

import Data.Array
import Control.Monad.Trans.Reader
import qualified OurByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Track (veryRawReadTrack, rawTrackToTileArray)
import Utils
import LapTrace
import Composition
import qualified Parameters as Pm

writePngOutput :: Pm.RenderingParameters -> FilePath -> IO ()
writePngOutput params trkPath = do
    trkBS <- LB.readFile trkPath
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    let willRenderIndices = Pm.drawIndices params
        renWidthInTiles = if willRenderIndices then 32 else 30
        renWidth = renWidthInTiles * Pm.pixelsPerTile params
        outType = Pm.outputType params
        outFile = case outType of
            PNG -> "stunts-cartography-map-tmp.png"
            SVG -> "stunts-cartography-map-tmp.svg"
            _   -> "stunts-cartography-map" --Nonsense
    fst . renderDia Cairo (CairoOptions outFile (Width renWidth) outType False) $
        --TODO: Restore the capability of tracing paths.
        (if Pm.drawGridLines params then gridLines else mempty)
        <>
        (if willRenderIndices then renderIndices else mempty)
        <>
        runReader (renderMap tiles) params


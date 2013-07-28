{-# LANGUAGE NoMonomorphismRestriction #-}
module Output where

import Data.Array
import qualified OurByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Core
import Track (veryRawReadTrack, rawTrackToTileArray)
import Utils
import LapTrace
import Composition

writePngOutput :: IO ()
writePngOutput = do
    trkBS <- LB.readFile "data/ZCT135.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    trDat <- readFile "data/135zdup.dat"
    let lapTrace = readRawTrace trDat
        lapPath = pathFromTrace lapTrace
    fst . renderDia Cairo (CairoOptions "test.png" (Width 960) PNG False) $
        simpleRenderTracePath lapPath
        <>
        gridLines
        <>
        renderIndices
        <>
        renderMap tiles


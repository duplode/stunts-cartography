{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Array
import qualified OurByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Track (veryRawReadTrack, rawTrackToTileArray)
import Utils
import LapTrace
import Composition

main = do
    trkBS <- LB.readFile "data/ZCT135.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    --putStrLn . show $ tilArr ! (4, 7)
    trDat <- readFile "data/135zdup.dat"
    let lapTrace = readRawTrace trDat
        lapPath = pathFromTrace lapTrace
    defaultMain $
        --renderTracePathWithCars lapPath
        -- <>
        simpleRenderTracePath lapPath
        <>
        gridLines
        <>
        renderIndices
        <>
        renderMap tiles


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
import Data.Ratio ((%))

main = do
    trkBS <- LB.readFile "ZCT070.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
        baseMap =
            gridLines
            <>
            renderMap tiles
    trDat <- readFile "070zgut.dat"
    let lapTrace = readRawTrace trDat
        lapPath = pathFromTrace lapTrace
        nSteps = (length . concat $ pathVertices lapPath) - 1
    animMain $
        animatedTrace lapPath
        # stretch (fromIntegral nSteps % 20)
        <> pure baseMap

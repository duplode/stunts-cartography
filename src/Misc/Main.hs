{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Array
import Control.Monad.Trans.Reader
import qualified Util.ByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Track (veryRawReadTrack, rawTrackToTileArray)
import Util.Misc
import LapTrace
import Composition
import qualified Parameters as Pm

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
        runReader (renderMap tiles) (Pm.defaultRenderingParameters)


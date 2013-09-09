{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Array
import Control.Monad.RWS hiding ((<>))
import qualified Util.ByteString as LB
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Track (veryRawReadTrack, rawTrackToTileArray)
import Util.Misc
import LapTrace.Legacy.LapTrace
import Composition
import qualified Parameters as Pm
import Data.Ratio ((%))

main = do
    trkBS <- LB.readFile "data/ZCT070.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
        baseMap =
            gridLines
            <>
            (fst3 $ runRWS (renderMap tiles) Pm.def Pm.def)
    trDat <- readFile "data/070zgut.dat"
    let lapTrace = readRawTrace trDat
        lapPath = pathFromTrace lapTrace
        nSteps = (length . concat $ pathVertices lapPath) - 1
    animMain $
        animatedTrace lapPath
        # stretch (fromIntegral nSteps % 20)
        <> pure baseMap

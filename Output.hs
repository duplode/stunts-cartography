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
import qualified Parameters as Params

writePngOutput :: Params.RenderingParameters -> FilePath -> IO ()
writePngOutput params trkPath = do
    trkBS <- LB.readFile trkPath
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    fst . renderDia Cairo (CairoOptions "test.png" (Width 960) PNG False) $
        --TODO: Restore the capability of tracing paths.
        gridLines
        <>
        renderIndices
        <>
        runReader (renderMap tiles) params


{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Array
import Control.Monad.Instances
import qualified OurByteString as LB
import Data.List (groupBy, splitAt, unfoldr)
import Data.Function (on)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track
import Pics
import Utils

main = do
    trkBS <- LB.readFile "CYDONIA.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
    --putStrLn . show $ tilArr ! (4, 7) -- Why are the indices swapped?
    let rows = map snd <$> splitAtEvery30th (assocs tilArr)
        terrRows = cat unitX <$> (map getTerrainPic <$> rows)
        terrFull = cat unitY terrRows
        elmsRows = cat' unitX with { sep = 1, catMethod = Distrib }
            <$> (map getTilePic <$> rows)
        elmsFull = cat' unitY with { sep = 1, catMethod = Distrib } elmsRows
    defaultMain $
        (elmsFull <> terrFull)
        # alignBL
        # atop gridLines

gridLines =
    vcat' with { sep = 1 } (replicate 31 $ hrule 30) # alignBL
    <> hcat' with { sep = 1 } (replicate 31 $ vrule 30) # alignBL

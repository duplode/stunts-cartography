{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Array
import Control.Monad.Instances
import qualified Data.ByteString.Lazy as LB
import Data.List (groupBy)
import Data.Function (on)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Track
import Pics

main = do
    trkBS <- LB.readFile "ZCT106.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
    --putStrLn . show $ tilArr ! (4, 7) -- Why are the indices swapped?
    let rows = map snd <$> groupBy ((==) `on` (fst . fst)) (assocs tilArr)
        -- TODO: Do this in a single pass (can arrows help?)
        terrRows = cat unitX <$> (map getTerrainPic <$> rows)
        terrFull = cat unitY terrRows
        elmsRows = cat' unitX with { sep = 1, catMethod = Distrib }
            <$> (map getTilePic <$> rows)
        elmsFull = cat' unitY with { sep = 1, catMethod = Distrib } elmsRows
    defaultMain $ elmsFull <> terrFull

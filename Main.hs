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
    trkBS <- LB.readFile "ZCT106.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
    --putStrLn . show $ tilArr ! (4, 7) -- Why are the indices swapped?
    let rows = map snd <$> splitAtEvery30th (assocs tilArr)
        -- TODO: Do this in a single pass (can arrows help?)
        terrRows = cat unitX <$> (map getTerrainPic <$> rows)
        terrFull = cat unitY terrRows
        elmsRows = cat' unitX with { sep = 1, catMethod = Distrib }
            <$> (map getTilePic <$> rows)
        elmsFull = cat' unitY with { sep = 1, catMethod = Distrib } elmsRows
    defaultMain $
        (elmsFull <> terrFull)
        # centerXY
        # atop (vcat' with { sep = 1 } (replicate 29 $ hrule 30) # centerY)
        # atop (hcat' with { sep = 1 } (replicate 29 $ vrule 30) # centerX)


-- Hand-rolled:
{-
group30TileRows :: [((Int, Int), Tile)] -> [] [((Int, Int), Tile)]
group30TileRows arrAscs =
    map fst . takeWhile (not . null . fst) . drop 1
    . iterate (splitAt 30 . snd) $ ([], arrAscs)

obviousGroupRows :: [((Int, Int), Tile)] -> [] [((Int, Int), Tile)]
obviousGroupRows = groupBy ((==) `on` (fst . fst))
-}

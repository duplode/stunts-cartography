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
    trkBS <- LB.readFile "CYDONIA.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
    putStrLn . show $ tilArr ! (6, 6) -- Why are the indices swapped?
    let rows = map snd <$> groupBy ((==) `on` (fst . fst)) (assocs tilArr)
        -- TODO: Do this in a single pass (can arrows help?)
        terrRows = cat (r2 (1, 0)) <$> (map getTerrainPic <$> rows)
        terrFull = cat (r2 (0, 1)) terrRows
        elmsRows = cat (r2 (1, 0)) <$> (map getTilePic <$> rows)
        elmsFull = cat (r2 (0, 1)) elmsRows
    defaultMain $ elmsFull `atop` terrFull

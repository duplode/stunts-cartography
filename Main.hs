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
    putStrLn . show $ tilArr ! (19, 24)
    let rows = map snd <$> groupBy ((==) `on` (fst . fst)) (assocs tilArr)
        diaRows = cat (r2 (1, 0)) <$> (map getTerrainPic <$> rows)
        diaFull = cat (r2 (0, 1)) diaRows
    defaultMain diaFull

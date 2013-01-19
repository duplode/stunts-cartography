import Data.Array
import Control.Monad.Instances
import qualified Data.ByteString.Lazy as LB
import Track

main = do
    trkBS <- LB.readFile "CYDONIA.TRK"
    let rawTrk = veryRawReadTrack trkBS
    let tilArr = rawTrackToTileArray rawTrk
    putStrLn . show $ tilArr ! (19, 24)

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
    trkBS <- LB.readFile "VISEGRAD.TRK"
    let rawTrk = veryRawReadTrack trkBS
        tilArr = rawTrackToTileArray rawTrk
    --putStrLn . show $ tilArr ! (4, 7) -- Why are the indices swapped?
    let
        tiles = map snd $ assocs tilArr
        terrRows = beneath plainStripe .
            catTiles <$> (map getTerrainPic <$> splitAtEvery30th tiles)
        terrFull = catRows terrRows
        (seTiles, leTiles) = separateTilesBySize tiles
        seRows = catTiles <$> (map getTilePic <$> splitAtEvery30th seTiles)
        seFull = catRows seRows
        leRows = catTiles <$> (map getTilePic <$> splitAtEvery30th leTiles)
        leFull = catRows leRows
        elmsFull = seFull <> leFull
    defaultMain $
        gridLines
        <> cat unitX [yIndices, strutX 30, yIndices]
        <> cat unitY [xIndices, strutY 30, xIndices]
        <> (elmsFull <> terrFull) # alignBL

catTiles = cat' unitX with { sep = 1, catMethod = Distrib }
catRows = cat' unitY with { sep = 1, catMethod = Distrib }

plainStripe = square 1 # scaleX 30 # translateX 14.5 # fc plainCl

separateTilesBySize :: [Tile] -> ([Tile], [Tile])
separateTilesBySize = unzip . map separate
    where
    separate tl = case getAbstractTileSize tl of
        Large -> (blankTile, tl)
        _     -> (tl, blankTile)

gridLines =
    vcat' with { sep = 1 } (replicate 31 $ hrule 30) # alignBL
    <> hcat' with { sep = 1 } (replicate 31 $ vrule 30) # alignBL

xIndices =
    hcat $ map indexCell [0..29] # alignTL

yIndices =
    cat unitY (map indexCell [0..29]) # alignBR

indexCell n =
    square 1 # lw 0
    # atop (text (show n) # scale 0.5)

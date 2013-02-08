{-# LANGUAGE NoMonomorphismRestriction #-}
module Composition
    ( renderMap
    , renderIndices
    , gridLines
    ) where

import Diagrams.Prelude
import Track
import Pics
import Utils
import Palette (plainCl)

renderTerrain tiles =
    let terrRows = beneath plainStripe .
            catTiles <$> (map getTerrainPic <$> splitAtEvery30th tiles)
    in catRows terrRows

renderElements tiles =
    let makeElementRows ts = catTiles
            <$> (map getTilePic <$> splitAtEvery30th ts)
        (seTiles, leTiles) = separateTilesBySize tiles
    in (catRows . makeElementRows $ seTiles)
    <> (catRows . makeElementRows $ leTiles)

renderMap tiles =
    (renderElements tiles <> renderTerrain tiles) # alignBL

renderIndices =
    cat unitX [yIndices, strutX 30, yIndices]
    <> cat unitY [xIndices, strutY 30, xIndices]

gridLines =
    vcat' with { sep = 1 } (replicate 31 $ hrule 30) # alignBL
    <> hcat' with { sep = 1 } (replicate 31 $ vrule 30) # alignBL

catTiles = cat' unitX with { sep = 1, catMethod = Distrib }
catRows = cat' unitY with { sep = 1, catMethod = Distrib }

plainStripe = square 1 # scaleX 30 # translateX 14.5 # fc plainCl # lw 0

separateTilesBySize :: [Tile] -> ([Tile], [Tile])
separateTilesBySize = unzip . map separate
    where
    separate tl = case getAbstractTileSize tl of
        Large -> (blankTile, tl)
        _     -> (tl, blankTile)

xIndices =
    hcat $ map indexCell [0..29] # alignTL

yIndices =
    cat unitY (map indexCell [0..29]) # alignBR

indexCell n =
    square 1 # lw 0
    # atop (text (show n) # scale 0.5)

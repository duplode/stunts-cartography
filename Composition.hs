{-# LANGUAGE NoMonomorphismRestriction #-}
module Composition
    ( renderMap
    , renderIndicesIfRequired
    , gridLines
    ) where

import Control.Monad (liftM)
import Control.Monad.Trans.Reader (asks)
import Diagrams.Prelude
import Track
import Pics
import Utils
import Palette (plainCl)
import qualified Parameters as Pm

renderTerrain tiles =
    let terrRows = beneath plainStripe .
            catTiles <$> (map getTerrainPic <$> splitAtEvery30th tiles)
    in catRows terrRows

renderElements tiles = do
    let makeElementRows ts = (catTiles <$>)
            `liftM` sequence (sequence . map getTilePic <$> splitAtEvery30th ts)
        (seTiles, leTiles) = separateTilesBySize tiles
    smallElementRows <- makeElementRows seTiles
    largeElementRows <- makeElementRows leTiles
    return $ catRows smallElementRows <> catRows largeElementRows

renderMap tiles = do
    let renderedTerrain = renderTerrain tiles
    renderedElements <- renderElements tiles
    return $ (renderedElements <> renderedTerrain) # alignBL

-- Only indices have to be deep-clipped here, as they are placed around, and
-- not just above, the base map.
renderIndices = do
    boundedXIndices <- xIndices <$> asks Pm.xTileBounds
    boundedYIndices <- yIndices <$> asks Pm.yTileBounds
    (deltaX, deltaY) <- asks Pm.deltaTileBounds
    return $
        cat unitX [boundedYIndices, strutX deltaX, boundedYIndices]
        <> cat unitY [boundedXIndices, strutY deltaY, boundedXIndices]

renderIndicesIfRequired = do
    required <- asks Pm.drawIndices
    if required
        then renderIndices
        else return mempty

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

xIndices (xMin, xMax) =
    hcat $ map indexCell [xMin..xMax] # alignTL

yIndices (yMin, yMax) =
    cat unitY (map indexCell [yMin..yMax]) # alignBR

indexCell n =
    square 1 # lw 0
    # atop (text (show n) # scale 0.5)

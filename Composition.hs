{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Composition
    ( renderMap
    , renderIndicesIfRequired
    , gridLines
    ) where

import Control.Monad (liftM)
import Control.Monad.RWS hiding ((<>))
import qualified Data.Map as M (lookup, insert)
import Diagrams.Prelude
import Track
import Pics
import Utils
import Palette (plainCl)
import qualified Parameters as Pm
import CartoM
import Types (BEDia)

renderTerrain :: [Tile] -> CartoM (Diagram BEDia R2)
renderTerrain tiles =
    let terrRows = map (beneath plainStripe . catTiles)
            `liftM` mapM (mapM getCachedTerrPic) (splitAtEvery30th tiles)
    in catRows <$> terrRows
{-
renderTerrain :: [Tile] -> CartoM (Diagram BEDia R2)
renderTerrain tiles = do
    let tileRows = splitAtEvery30th tiles
-}

renderElements :: [Tile] -> CartoM (Diagram BEDia R2)
renderElements tiles = do
    let makeElementRows ts = map catTiles
            `liftM` mapM (mapM getCachedElemPic) (splitAtEvery30th ts)
        (seTiles, leTiles) = separateTilesBySize tiles
    smallElementRows <- makeElementRows seTiles
    largeElementRows <- makeElementRows leTiles
    return $ catRows smallElementRows <> catRows largeElementRows

getCachedElemPic :: Tile -> CartoM (Diagram BEDia R2)
getCachedElemPic tile = do
    let el = tileElement tile
    mDia <- M.lookup el <$> gets Pm.elementCache
    case mDia of
        Just dia -> return dia
        Nothing -> do
            newDia <- getTilePic tile
            modify $ Pm.insertIntoElementCache el newDia
            return newDia

getCachedTerrPic :: Tile -> CartoM (Diagram BEDia R2)
getCachedTerrPic tile = do
    let te = tileTerrain tile
    mDia <- M.lookup te <$> gets Pm.terrainCache
    case mDia of
        Just dia -> return dia
        Nothing -> do
            let newDia = getTerrainPic tile
            modify $ Pm.insertIntoTerrainCache te newDia
            return newDia

renderMap :: [Tile] -> CartoM (Diagram BEDia R2)
renderMap tiles = do
    renderedTerrain <- renderTerrain tiles
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

renderIndicesIfRequired :: CartoM (Diagram BEDia R2)
renderIndicesIfRequired = do
    required <- asks Pm.drawIndices
    if required
        then renderIndices
        else return mempty

gridLines :: (Monoid m, Semigroup m, TrailLike (QDiagram BEDia R2 m))
          => QDiagram BEDia R2 m
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

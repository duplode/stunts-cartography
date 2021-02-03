{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Util.Misc
import Pics.Palette (plainCl)
import qualified Parameters as Pm
import Types.CartoM
import Types.Diagrams (BEDia)

renderTerrain :: [Tile] -> CartoM (Diagram BEDia)
renderTerrain tiles = do
    omitBg <- asks Pm.transparentBg
    let bg = if omitBg then mempty else plainStripe
        terrRows = map (beneath bg . catTiles)
            `liftM` mapM (mapM getCachedTerrPic) (splitAtEvery30th tiles)
    catRows <$> terrRows
{-
renderTerrain :: [Tile] -> CartoM (Diagram BEDia R2)
renderTerrain tiles = do
    let tileRows = splitAtEvery30th tiles
-}

renderElements :: [Tile] -> CartoM (Diagram BEDia)
renderElements tiles = do
    let makeElementRows ts = map catTiles
            `liftM` mapM (mapM getCachedElemPic) (splitAtEvery30th ts)
        (seTiles, leTiles) = separateTilesBySize tiles
    smallElementRows <- makeElementRows seTiles
    largeElementRows <- makeElementRows leTiles
    return $ catRows smallElementRows <> catRows largeElementRows

getCachedElemPic :: Tile -> CartoM (Diagram BEDia)
getCachedElemPic tile = do
    let el = tileElement tile
    mDia <- M.lookup el <$> gets Pm.elementCache
    case mDia of
        Just dia -> return dia
        Nothing -> do
            newDia <- getTilePic tile
            modify $ Pm.insertIntoElementCache el newDia
            return newDia

getCachedTerrPic :: Tile -> CartoM (Diagram BEDia)
getCachedTerrPic tile = do
    let te = tileTerrain tile
    mDia <- M.lookup te <$> gets Pm.terrainCache
    case mDia of
        Just dia -> return dia
        Nothing -> do
            twoTone <- asks Pm.twoToneTerrain
            let newDia = getTerrainPic twoTone tile
            modify $ Pm.insertIntoTerrainCache te newDia
            return newDia

renderMap :: [Tile] -> CartoM (Diagram BEDia)
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

renderIndicesIfRequired :: CartoM (Diagram BEDia)
renderIndicesIfRequired = do
    required <- asks Pm.drawIndices
    if required
        then renderIndices
        else return mempty

gridLines :: (Monoid' m, TrailLike (QDiagram BEDia V2 Double m))
          => QDiagram BEDia V2 Double m
gridLines =
    vcat' (with & sep .~ 1) (replicate 31 $ hrule 30 # lwG 0.01) # alignBL
    <> hcat' (with & sep .~ 1) (replicate 31 $ vrule 30 # lwG 0.01) # alignBL

catTiles = cat' unitX (with & sep .~ 1 & catMethod .~ Distrib)
catRows = cat' unitY (with & sep .~ 1 & catMethod .~ Distrib)

plainStripe = square 1 # scaleX 30 # translateX 14.5 # fc plainCl # lwG 0

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
    square 1 # lwG 0
    # atop (text (show n) # scale 0.5)

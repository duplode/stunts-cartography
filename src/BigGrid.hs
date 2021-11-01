{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module BigGrid
    ( runRenderBigGrid
    , testPaths
    ) where

import Data.Array
import qualified Data.ByteString.Lazy as LB
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except
import Data.List.Extra (chunksOf)
import Data.Default.Class
import System.Directory
import System.FilePath

import Diagrams.Prelude

import Output (wholeMapDiagram)
import Track
import qualified Parameters as Pm
import Types.CartoM
import Util.Diagrams.Backend (OutputType(..), B, renderBE
    , widthConversionFactor)

testPaths = ["data/ZCT135.TRK", "data/VISEGRAD.TRK", "data/CYDONIA.TRK", "data/ZCT128.TRK"
    , "data/FONYOD.TRK"]

-- Similar to Viewer.setup.runRenderMap
runRenderBigGrid :: [FilePath] -> IO ()
runRenderBigGrid paths = do
    absPaths <- mapM makeAbsolute paths

    tiledTracks <- readTiles absPaths
    eitRender <- runExceptT $ do
        runRWST (writeImageOutput tiledTracks) bigGridParameters def
    case eitRender of
        Left errorMsg -> error $ "BigGrid.runRenderBigGrid: " ++ errorMsg
        Right _ -> return ()

-- Similar to Output.writeImageOutput, but factored differently.
writeImageOutput :: MonadIO m
                 => [[Tile]]
                 -> CartoT (ExceptT String m) ()
writeImageOutput tiledTracks = do
    outType <- asks Pm.outputType
    renWidthInTiles <- (\drawIx -> if drawIx then (2+) else id)
        <$> asks Pm.drawIndices <*> asks (fst . Pm.deltaTileBounds)
    renWidth <- (renWidthInTiles * widthConversionFactor outType *)
        <$> asks Pm.pixelsPerTile

    -- TODO: Set the destination path properly
    dir <- liftIO getCurrentDirectory
    let outRelPath = case outType of
            PNG -> "stunts-cartography-biggrid.png"
            SVG -> "stunts-cartography-biggrid.svg"
        outFile = dir </> outRelPath

    let mapsPerRow = (floor . sqrt . fromIntegral) (length tiledTracks)

    wholeMaps <- mapM wholeMapDiagram tiledTracks
    let bigGrid = arrangeBigGrid mapsPerRow wholeMaps
    liftIO $ renderBE outFile (mkWidth renWidth) bigGrid

readTiles :: MonadIO m => [FilePath] -> m [[Tile]]
readTiles paths = forM paths $ \path -> do
    trkBS <- liftIO $ LB.readFile path
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr
    return tiles

arrangeBigGrid :: Int -> [QDiagram B V2 Double Any] -> QDiagram B V2 Double Any
arrangeBigGrid n diags = chunksOf n diags
    # fmap hcat
    # vcat

-- TODO: This should be configurable to some extent.
bigGridParameters :: Pm.RenderingParameters
bigGridParameters = def
    { Pm.pixelsPerTile = 32
    , Pm.drawGridLines = True
    , Pm.drawIndices = False
    }

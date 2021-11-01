{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module BigGrid
    ( subMain
    , Options (..)
    , opts
    ) where

import Data.Array
import qualified Data.ByteString.Lazy as LB
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except
import Data.List.Extra (chunksOf)
import Data.Default.Class
import System.Directory
import System.FilePath
import qualified Options.Applicative as Opts

import Diagrams.Prelude hiding (Options)

import Output (wholeMapDiagram)
import Track
import qualified Parameters as Pm
import Types.CartoM
import Util.Diagrams.Backend (OutputType(..), B, renderBE
    , widthConversionFactor, defaultOutputType)

-- TODO: This module is still very rough around the edges.
subMain :: Options -> IO ()
subMain = runRenderBigGrid

data Options = Options
    { outputType :: OutputType
    , pixelsPerTile :: Double
    , inputFile :: FilePath
    , outputFile :: FilePath
    }

-- TODO: Use opponentFlag once we get repldump to export opponent data.
baseOpts :: Opts.Parser Options
baseOpts = Options <$> svgFlag <*> pxptOption <*> argInputFile <*> argOutputFile

argInputFile :: Opts.Parser FilePath
argInputFile = Opts.argument Opts.str
    ( Opts.help "List of track files to use (text file, one track per line)"
    <> Opts.metavar "INPUT"
    )

argOutputFile :: Opts.Parser FilePath
argOutputFile = Opts.argument Opts.str
    ( Opts.help "Output image file"
    <> Opts.metavar "OUTPUT"
    )

svgFlag :: Opts.Parser OutputType
svgFlag = Opts.flag defaultOutputType SVG
    ( Opts.long "svg"
    <> Opts.help "Generate SVG image"
    )

pxptOption :: Opts.Parser Double
pxptOption = Opts.option Opts.auto
    ( Opts.long "tile"
    <> Opts.metavar "N"
    <> Opts.value 32
    <> Opts.help "Tile size in pixels"
    )

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts
    ( Opts.fullDesc
    <> Opts.progDesc "Arrange multiple track maps in a grid"
    )

-- Similar to Viewer.setup.runRenderMap
runRenderBigGrid :: Options -> IO ()
runRenderBigGrid o = do
    paths <- lines <$> readFile (inputFile o)
    absPaths <- mapM makeAbsolute paths

    tiledTracks <- readTiles absPaths

    let params = bigGridParameters
            { Pm.outputType = outputType o
            , Pm.pixelsPerTile = pixelsPerTile o
            }

    eitRender <- runExceptT $ do
        runRWST (writeImageOutput o tiledTracks) params def
    case eitRender of
        Left errorMsg -> error $ "BigGrid.runRenderBigGrid: " ++ errorMsg
        Right _ -> return ()

-- Similar to Output.writeImageOutput, but factored differently.
writeImageOutput :: MonadIO m
                 => Options
                 -> [[Tile]]
                 -> CartoT (ExceptT String m) ()
writeImageOutput o tiledTracks = do
    let mapsPerRow = (floor . sqrt . fromIntegral) (length tiledTracks)

    -- The tile bounds are ignored, as we only render full maps here.
    outType <- asks Pm.outputType
    renWidthInTiles <- (\drawIx -> if drawIx then (2+) else id)
        <$> asks Pm.drawIndices <*> pure (30 * fromIntegral mapsPerRow)
    renWidth <- (renWidthInTiles * widthConversionFactor outType *)
        <$> asks Pm.pixelsPerTile

    let outFile = outputFile o

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

-- TODO: At a minimum, drawGridLines should be configurable.
bigGridParameters :: Pm.RenderingParameters
bigGridParameters = def
    { Pm.drawGridLines = True
    , Pm.drawIndices = False
    }

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module BigGrid
    ( subMain
    , Options (..)
    , opts
    ) where

import Data.Array
import qualified Data.ByteString.Lazy as LB
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Except
import Data.List (intersperse)
import Data.List.Extra (chunksOf)
import Data.Char (toLower)
import Data.Default.Class
import System.Directory
import System.FilePath
import qualified Options.Applicative as Opts
import Text.Printf

import Diagrams.Prelude hiding (Options)

import Output (wholeMapDiagram)
import Track
import qualified Parameters as Pm
import Types.CartoM
import Util.Diagrams.Backend (OutputType(..), B, renderBE
    , widthConversionFactor, defaultOutputType, alternativeOutputTypes)

-- TODO: This module is still very rough around the edges.
subMain :: Options -> IO ()
subMain = runRenderBigGrid

data Options = Options
    { drawGridLines :: Bool
    , rowSize :: Maybe Int
    , pixelsPerTile :: Double
    , inputFile :: FilePath
    , outputFile :: FilePath
    }

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> gridLinesSwitch <*> rowSizeOption <*> pxptOption
    <*> argInputFile <*> argOutputFile

argInputFile :: Opts.Parser FilePath
argInputFile = Opts.argument Opts.str
    ( Opts.help "List of track files to use (text file, one track per line)"
    <> Opts.metavar "INPUT"
    )

argOutputFile :: Opts.Parser FilePath
argOutputFile = Opts.argument Opts.str
    ( Opts.help (printf "Output image file (available formats: %s)" fmts)
    <> Opts.metavar "OUTPUT"
    )
    where
    fmts = concat . intersperse ", " . map show $
        defaultOutputType : alternativeOutputTypes

gridLinesSwitch :: Opts.Parser Bool
gridLinesSwitch = Opts.switch
    ( Opts.long "lines"
    <> Opts.help "Draw tile grid lines"
    )

rowSizeOption :: Opts.Parser (Maybe Int)
rowSizeOption = Opts.option (Just <$> Opts.auto)
    ( Opts.long "row"
    <> Opts.metavar "N"
    <> Opts.value Nothing
    <> Opts.help "Number of maps per grid row (approximates a square by default)"
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

data GridObject = EmptyCell | TrackTiles [Tile]

-- Similar to Viewer.setup.runRenderMap
runRenderBigGrid :: Options -> IO ()
runRenderBigGrid o = do
    paths <- lines <$> readFile (inputFile o)
    tiledTracks <- readTiles paths

    -- This is still needed for the width correction.
    let outType = case toLower <$> takeExtension (outputFile o) of
            ".svg" -> SVG
            ".png" -> PNG
            _ -> defaultOutputType
        params = bigGridParameters
            { Pm.outputType = outType
            , Pm.pixelsPerTile = pixelsPerTile o
            , Pm.drawGridLines = drawGridLines o
            }

    eitRender <- runExceptT $ do
        runRWST (writeImageOutput o tiledTracks) params def
    case eitRender of
        Left errorMsg -> error $ "BigGrid.runRenderBigGrid: " ++ errorMsg
        Right _ -> return ()

-- Similar to Output.writeImageOutput, but factored differently.
writeImageOutput :: MonadIO m
                 => Options
                 -> [GridObject]
                 -> CartoT (ExceptT String m) ()
writeImageOutput o tiledTracks = do
    let mapsPerRow = case rowSize o of
            Just n -> n
            Nothing -> (floor . sqrt . fromIntegral) (length tiledTracks)

    -- The tile bounds are ignored, as we only render full maps here.
    outType <- asks Pm.outputType
    renWidthInTiles <- (\drawIx -> if drawIx then (2+) else id)
        <$> asks Pm.drawIndices <*> pure (30 * fromIntegral mapsPerRow)
    renWidth <- (renWidthInTiles * widthConversionFactor outType *)
        <$> asks Pm.pixelsPerTile

    let outFile = outputFile o

    wholeMaps <- forM tiledTracks $ \case
        TrackTiles tiles -> wholeMapDiagram tiles
        EmptyCell -> return cellFiller
    let bigGrid = arrangeBigGrid mapsPerRow wholeMaps
    liftIO $ renderBE outFile (mkWidth renWidth) bigGrid

-- TODO: If we ever want to fill empty cells with anything, the list of grid
-- objects will have to be padded to the visual grid size.
readTiles :: MonadIO m => [FilePath] -> m [GridObject]
readTiles paths = forM paths $ \path -> do
    let skipped = null path
    if skipped
        then return EmptyCell
        else do
            -- TODO: Existence checks, etc.
            trkBS <- liftIO $ LB.readFile path
            let rawTrk = veryRawReadTrack trkBS
                horizon = horizonFromRawTrack rawTrk
                tilArr = rawTrackToTileArray rawTrk
                tiles = map snd (assocs tilArr)
            return (TrackTiles tiles)

arrangeBigGrid :: Int -> [QDiagram B V2 Double Any] -> QDiagram B V2 Double Any
arrangeBigGrid n diags = chunksOf n diags
    # fmap hcat
    # vcat

cellFiller :: QDiagram B V2 Double Any
cellFiller = (strutX 30 # centerX <> strutY 30 # centerY) # alignL

-- TODO: Add some extra configurability.
bigGridParameters :: Pm.RenderingParameters
bigGridParameters = def
    { Pm.drawIndices = False
    }

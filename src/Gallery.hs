{-# LANGUAGE LambdaCase #-}
module Gallery
    ( subMain
    , Options (..)
    , opts
    ) where

import Data.Default.Class
import Text.Printf (printf)
import Data.Array (assocs)
import Control.Monad (when, unless)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Char (toUpper)
import System.FilePath (takeFileName, takeBaseName, takeExtension
    , (</>), (<.>))
import System.Directory (doesFileExist, doesDirectoryExist
    , createDirectoryIfMissing)
import System.Directory.Extra (listDirectories, listFiles)
import qualified Data.ByteString.Lazy as LB

import Diagrams.Prelude (mkWidth, def)
import qualified Options.Applicative as Opts

import Output (wholeMapDiagram)
import Track
import Types.CartoM
import qualified Parameters as Pm
import Util.Diagrams.Backend (OutputType(..), B, renderBE
    , widthConversionFactor, defaultOutputType, alternativeOutputTypes)

subMain :: Options -> IO ()
subMain = runRenderGallery

-- TODO: Consider deduplicating wrt BigGrid
data Options = Options
    { inputDir :: FilePath
    , outputDir :: Maybe FilePath
    , drawGrid :: Bool
    , drawIndices :: Bool
    , renderStyle :: RenderStyle
    , pixelsPerTile :: Double
    , generateSVG :: Bool
    }

baseOpts :: Opts.Parser Options
baseOpts = Options
    <$> argInputDir <*> outputDirOption
    <*> gridSwitch <*> indicesSwitch <*> renderStyleOption
    <*> pxptOption <*> svgSwitch

argInputDir :: Opts.Parser FilePath
argInputDir = Opts.argument Opts.str
    ( Opts.help "Directory to read track files from (defaults to current)"
    <> Opts.metavar "INPUTDIR"
    <> Opts.value "."
    )

outputDirOption :: Opts.Parser (Maybe FilePath)
outputDirOption = Opts.option (Just <$> Opts.str)
    ( Opts.short 'o'
    <> Opts.long "output-dir"
    <> Opts.help "Output directory (created if not existing, defaults to INPUTDIR/galcarto)"
    <> Opts.metavar "OUTPUTDIR"
    <> Opts.value Nothing
    )

gridSwitch :: Opts.Parser Bool
gridSwitch = Opts.switch
    ( Opts.long "grid"
    <> Opts.help "Draw tile grid lines"
    )

indicesSwitch :: Opts.Parser Bool
indicesSwitch = Opts.switch
    ( Opts.long "indices"
    <> Opts.help "Draw tile indices"
    )


data RenderStyle = Basic | Wider | Sloping | Classic
    deriving (Read)

renderStyleOption :: Opts.Parser RenderStyle
renderStyleOption = Opts.option Opts.auto
    ( Opts.long "style"
    <> Opts.metavar "STYLE"
    <> Opts.value Basic
    <> Opts.help "Tile rendering style (one of Basic, Wider, Sloping and Classic)"
    )

pxptOption :: Opts.Parser Double
pxptOption = Opts.option Opts.auto
    ( Opts.long "scale"
    <> Opts.metavar "N"
    <> Opts.value 32
    <> Opts.help "Tile size in pixels"
    )

svgSwitch :: Opts.Parser Bool
svgSwitch = Opts.switch
    ( Opts.long "svg"
    <> Opts.help "Generate SVG output (with the Carto build)"
    )

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts
    ( Opts.fullDesc
    <> Opts.progDesc "Render all tracks from a directory"
    )

startingParams :: RenderStyle -> Pm.RenderingParameters
startingParams = \case
    Basic -> Pm.defaultRenderingParameters
    Wider -> Pm.widerRoadsRenderingParameters
    Sloping -> Pm.slopingRampsRenderingParameters
    Classic -> Pm.classicRenderingParameters

runRenderGallery :: Options -> IO ()
runRenderGallery o = do
    let idir = inputDir o
    idirExists <- doesDirectoryExist idir
    unless idirExists $ error "Input directory does not exist"
    trkFiles <- getFileListing (inputDir o)
    when (null trkFiles) $ error "No track files in input directory" 
    let trkPaths = map (idir </>) trkFiles
        odir = maybe (idir </> "galcarto") id (outputDir o)
    createDirectoryIfMissing True odir

    let outType = if (SVG `elem` alternativeOutputTypes) && generateSVG o
            then SVG
            else defaultOutputType
        params = (startingParams (renderStyle o))
            { Pm.outputType = outType
            , Pm.pixelsPerTile = pixelsPerTile o
            , Pm.drawGridLines = drawGrid o
            , Pm.drawIndices = drawIndices o
            , Pm.outputDirectory = odir
            }

    eitRender <- runExceptT $ do
        runCartoT (mapM_ writeImageFromTrk trkPaths) params def
    case eitRender of
        Left errorMsg -> error $ "BigGrid.runRenderGallery: " ++ errorMsg
        Right _ -> return ()

-- Adapted from Output.writeImageFromTrk
writeImageFromTrk :: MonadIO m
                  => FilePath -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageFromTrk trkPath = do
    -- TODO: Consider adding more validity checks.
    trkExists <- liftIO $ doesFileExist trkPath
    unless trkExists $ throwError (trkPath ++ " does not exist")
    trkData <- liftIO $ LB.readFile trkPath
    writeImageOutput (takeBaseName trkPath) trkData

-- Adapted from Output.writeImageOutput
writeImageOutput :: MonadIO m
                 => String -> LB.ByteString
                 -> CartoT (ExceptT String m) Pm.PostRenderInfo
writeImageOutput trackName trkBS = do
    let rawTrk = veryRawReadTrack trkBS
        horizon = horizonFromRawTrack rawTrk
        tilArr = rawTrackToTileArray rawTrk
        tiles = map snd $ assocs tilArr

    outType <- asks Pm.outputType
    renWidthInTiles <- (\drawIx -> if drawIx then (2+) else id)
        <$> asks Pm.drawIndices <*> asks (fst . Pm.deltaTileBounds)
    renWidth <- (renWidthInTiles * widthConversionFactor outType *)
        <$> asks Pm.pixelsPerTile
    outDir <- asks Pm.outputDirectory

    postInfo <- do
        let outRelPath = case outType of
                PNG -> trackName <.> "png"
                SVG -> trackName <.> "svg"
            outFile = outDir </> outRelPath

        wholeMap <- wholeMapDiagram tiles
        liftIO $ renderBE outFile (mkWidth renWidth) wholeMap

        modify Pm.incrementNumberOfRuns
        return Pm.PostRenderInfo
            { Pm.renderedTrackHorizon = horizon
            , Pm.trackName = trackName
            , Pm.trackData = trkBS
            , Pm.outputRelPath = outRelPath
            , Pm.flipbookRelPath = Nothing
            }

    -- We might want to log some extra information eventually.
    {-
    tell . Pm.logFromString $ printf "Track name: %s\r\n" (Pm.trackName postInfo)
    tell . Pm.logFromString $ printf "Scenery: %s\r\n"
        (printHorizon (Pm.renderedTrackHorizon postInfo))
        -}

    return postInfo

-- Adapted from Widget.FilePathPicker
getFileListing :: FilePath -> IO [FilePath]
getFileListing dir = fmap (fmap takeFileName . filterTrk) $ do
    let dir' = blankDirToDot dir
    exists <- doesDirectoryExist dir'
    if exists
        then listFiles dir'
        else return []
    where
    filterTrk :: [FilePath] -> [FilePath]
    filterTrk = filter $ (== ".TRK") . takeExtension . map toUpper

    blankDirToDot :: FilePath -> FilePath
    blankDirToDot dir = if null dir then "." else dir

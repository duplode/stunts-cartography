{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
    ( main
    ) where

import Control.Monad
import qualified Control.Monad.RWS as RWS
import Control.Monad.Except
import Control.Exception (catch, SomeException)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import Data.List (sort)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory
    (doesFileExist, doesDirectoryExist, getDirectoryContents, getFileSize)
import System.FilePath ((</>), takeExtension, addExtension, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.Char (toUpper)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny (concatE, union, setter)
import Diagrams.Backend.Cairo (OutputType(..))

import Output
import qualified Util.ByteString as LB
import Track (Horizon(..), terrainTrkSimple)
import qualified Parameters as Pm
import Annotation (Annotation)
import Annotation.Parser (parseAnnotations, parseFlipbook)
import Types.CartoM
import Paths
import qualified Widgets.BoundedInput as BI
import Util.Threepenny.Alertify
import Util.Threepenny.JQueryAutocomplete

main :: IO ()
main = withSystemTempDirectory "stunts-cartography-" $ \tmpDir -> do
    (port, initDir) <- processOpts
    staticDir <- (</> "wwwroot") <$> getDataDir
    consoleGreeting $! port
    startGUI defaultConfig
        { jsPort = Just port
        , jsCustomHTML = Just "index.html"
        , jsStatic = Just staticDir
        } $ setup initDir tmpDir

-- Command line option processing.
data Flag = Port Int | InitDir FilePath | Help
    deriving (Show)

optionSpec :: [OptDescr Flag]
optionSpec =
    [ Option ['p'] ["port"]
        (ReqArg portp "PORT")  "port number (default 10000)"
    , Option ['d'] ["initial-dir"]
        (ReqArg InitDir "DIR") "directory initially selected (default ..)"
    , Option ['h'] ["help"]
        (NoArg Help)           "prints this help text."
    ]
    where
    portp = Port . fromMaybe (error $ "bad port number " ++ consoleHelp)
        . readMaybe

viewerOpts :: [String] -> IO ([Flag], [String])
viewerOpts argv =
    case getOpt Permute optionSpec argv of
        (o, [], [])  -> return (o, [])
        (_, _, errs) -> ioError $ userError $ concat errs ++ consoleHelp

consoleHelp :: String
consoleHelp = usageInfo helpHeader optionSpec
    where
    helpHeader = "Usage: sc-trk-viewer [OPTION]..."

processOpts :: IO (Int, FilePath)
processOpts = do
    (opts, _) <- viewerOpts =<< getArgs
    foldM (#) (10000, "..") $ map mkOptSetter opts
    where
    mkOptSetter :: Flag -> ((Int, FilePath) -> IO (Int, FilePath))
    mkOptSetter flag = case flag of
        Port p    -> return . (\(_, d) -> (p, d))
        InitDir d -> return . (\(p, _) -> (p, d))
        Help      -> const $ putStrLn consoleHelp >> exitSuccess

consoleGreeting :: Int -> IO ()
consoleGreeting port = do
    putStrLn $ "Welcome to Stunts Cartography"
        ++ maybe "" (' ':) versionString ++ "."
    putStrLn $ "Open your web browser and navigate to localhost:"
        ++ show port ++ " to begin."
    putStrLn ""
    when isPortableBuild $ do
        putStrLn "This is a portable build."
        putStrLn "If the interface doesn't load, check \
                 \if there is a wwwroot directory with"
        putStrLn "auxiliary files of the application \
                 \in the directory of the executable."
        putStrLn ""

setup :: FilePath -> FilePath -> Window -> UI ()
setup initDir tmpDir w = void $ do

    return w # set title "Stunts Cartography - Track Viewer"
    --UI.addStyleSheet w "viewer.css"
    --autocompleteSetup w "static/lib/"
    --alertifySetup w "static/lib/"

    -- Base directory and file selection, with autocompletion.
    let initialDir = initDir

        getDirListing :: FilePath -> IO [FilePath]
        getDirListing dir = fmap sort $ do
        -- Assuming we already checked that dir exists.
            getDirectoryContents dir
                >>= (filterM doesDirectoryExist
                    . map ((dotToBlankDir dir) </>))

        filterTrkRpl = filter $ (\x -> x == ".TRK" || x == ".RPL")
            . takeExtension . map toUpper

        blankDirToDot dir = if null dir then "." else dir
        dotToBlankDir dir = case dir of
            "." -> ""
            _ -> dir

        toFileListing :: Event FilePath -> Event [FilePath]
        toFileListing = unsafeMapIO getFileListing

        getFileListing :: FilePath -> IO [FilePath]
        getFileListing dir = fmap (sort . filterTrkRpl) $ do
            let dir' = blankDirToDot dir
            exists <- doesDirectoryExist dir'
            if exists
                then getDirectoryContents dir'
                        >>= (filterM $ doesFileExist . (dir' </>))
                else return []

    initialDirExists <- liftIO $ doesDirectoryExist initialDir
    initialDirListing <- liftIO $ if initialDirExists
        then getDirListing initialDir
        else return []
    initialFileListing <- liftIO $ getFileListing initialDir

    itxBasePath <-
        UI.input # set UI.type_ "text" # set UI.name "base-path-input"
            # set UI.id_ "base-path-input"
            # set value initialDir

    let eBaseDir = UI.valueChange itxBasePath
    bBaseDir <- initialDir `stepper` eBaseDir

    let completionDir :: FilePath -> IO (Maybe FilePath)
        completionDir dir = do
            let dir' = blankDirToDot dir
            exists <- doesDirectoryExist dir'
            if exists
                then return $ Just dir'
                else do
                    -- A trick so that completion does the right thing when
                    -- using Shift + arrow key and then pressing a character.
                    let dir'' = blankDirToDot $
                            (\xs -> guard (not $ null xs) >> init xs) dir'
                    exists' <- doesDirectoryExist dir''
                    return $ guard exists' >> Just dir''

        eExistingBaseDir = filterJust . unsafeMapIO completionDir $ eBaseDir
        eDirListing = unsafeMapIO getDirListing $ eExistingBaseDir
        eFileListing = unsafeMapIO getFileListing $ eBaseDir

    bDirListing <- initialDirListing `stepper` eDirListing
    bFileListing <- initialFileListing `stepper` eFileListing

    itxTrkPath <-
        UI.input # set UI.type_ "text" # set UI.name "trk-input"
            # set UI.id_ "trk-input"

    element itxBasePath
        # autocompleteInit
        # sink autocompleteArraySource bDirListing

    element itxTrkPath
        # autocompleteInit
        # sink autocompleteArraySource bFileListing

    -- Output type and tile resolution caption.
    selOutput <-
        UI.select # set UI.name "output-format-select"
            # set UI.id_ "output-format-select" #+
                [ UI.option # set UI.selected True #+ [string "PNG"]
                , UI.option #+ [string "SVG"]
                ]

    let eSelOutput = UI.selectionChange selOutput
    bOutType <- (intToOutputType . fromMaybe (-1) <$>)
        <$> (Just 0 `stepper` eSelOutput)

    let bPxPtText =
            let toPxPtText x =
                    case x of
                        SVG -> "Points per tile:"
                        _   -> "Pixels per tile:" -- PNG
            in toPxPtText <$> bOutType

    strPxPtPerTile <- string "" # sink text bPxPtText

    bidPxPtPerTile <- BI.new (8, 128)
    bPxPtPerTile   <- Pm.pixelsPerTile Pm.def `BI.userModel` bidPxPtPerTile

    -- Map bounds, grid and indices.

    let (defMinX, defMaxX) = Pm.xTileBounds Pm.def
        (defMinY, defMaxY) = Pm.yTileBounds Pm.def

        styleBoundsInput = BI.setTextInputSize 2
            . BI.formatBoundsCaption (const "")

    biiMinX <- BI.new (0, 29) # styleBoundsInput
    bMinX   <- defMinX `BI.userModel` biiMinX
    biiMaxX <- BI.new (0, 29) # styleBoundsInput
    bMaxX   <- defMaxX `BI.userModel` biiMaxX
    biiMinY <- BI.new (0, 29) # styleBoundsInput
    bMinY   <- defMinY `BI.userModel` biiMinY
    biiMaxY <- BI.new (0, 29) # styleBoundsInput
    bMaxY   <- defMaxY `BI.userModel` biiMaxY

    let ensureBoundOrder bounds@(z, w) = if z > w then (w, z) else bounds
        bBoundsX = ensureBoundOrder <$> (pure (,) <*> bMinX <*> bMaxX)
        bBoundsY = ensureBoundOrder <$> (pure (,) <*> bMinY <*> bMaxY)

    chkDrawGrid <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
            # set UI.id_ "grid-lines-chk"
    bDrawGrid <- Pm.drawGridLines Pm.def `stepper` UI.checkedChange chkDrawGrid
    currentValue bDrawGrid >>= (element chkDrawGrid #) . set UI.checked

    chkDrawIndices <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
            # set UI.id_ "grid-indices-chk"
    bDrawIndices <- Pm.drawIndices Pm.def `stepper` UI.checkedChange chkDrawIndices
    currentValue bDrawIndices >>= (element chkDrawIndices #) . set UI.checked

    chkTransparentBg <-
        UI.input # set UI.type_ "checkbox" # set UI.name "transparent-bg-chk"
            # set UI.id_ "transparent-bg-chk"
    bTransparentBg <- Pm.transparentBg Pm.def `stepper` UI.checkedChange chkTransparentBg
    currentValue bTransparentBg >>= (element chkTransparentBg #) . set UI.checked

    chkTwoToneTerrain <-
        UI.input # set UI.type_ "checkbox" # set UI.name "two-tone-chk"
            # set UI.id_ "two-tone-chk"
    bTwoToneTerrain <- Pm.twoToneTerrain Pm.def `stepper` UI.checkedChange chkTwoToneTerrain
    currentValue bTwoToneTerrain >>= (element chkTwoToneTerrain #) . set UI.checked

    -- Preset selection and ratio field initialization.
    let presetDefAndSetter :: (Pm.RenderingParameters -> a)
                           -> Event (Pm.RenderingParameters)
                           -> (a, Event (a -> a))
        presetDefAndSetter fParam eParams =
            (fParam Pm.def, setter $ fParam <$> eParams)

        trimFracPart n = (/ 10^n) . realToFrac . truncate . (* 10^n)
        presetDefAndSetterD fParam =
            (\(x, e) -> (trimFracPart 3 x, (trimFracPart 3 .) <$> e))
                . presetDefAndSetter fParam

    selPreset <-
        UI.select # set UI.name "style-preset-select"
            # set UI.id_ "style-preset-select" #+
                [ UI.option # set UI.selected True #+ [string "Default"]
                , UI.option #+ [string "Wider track"]
                , UI.option #+ [string "Sloping ramps"]
                , UI.option #+ [string "Traditional"]
                ]

    let eSelPreset = UI.selectionChange selPreset
    bPreset <- (intToPresetRenderingParams . fromMaybe (-1) <$>)
        <$> (Just 0 `stepper` eSelPreset)

    btnPreset <- UI.button #. "button" #+ [string "Set"]

    let ePreset = bPreset <@ UI.click btnPreset

    -- Rendering ratios
    let ratioModel fParam bi = do
            let (defRatio, eRatio) = presetDefAndSetterD fParam ePreset
            eRatio' <- BI.withRefresh bi eRatio
            BI.simpleModel defRatio eRatio' bi

    bidRoadW       <- BI.new (0.1, 0.5)
    bRoadW         <- Pm.roadWidth `ratioModel` bidRoadW
    bidBridgeH     <- BI.new (0, 0.5)
    bBridgeH       <- Pm.bridgeHeight `ratioModel` bidBridgeH
    bidBridgeRelW  <- BI.new (1, 3)
    bBridgeRelW    <- Pm.bridgeRelativeWidth `ratioModel` bidBridgeRelW
    bidBankingRelH <- BI.new (0.25, 1)
    bBankingRelH   <- Pm.bankingRelativeHeight `ratioModel` bidBankingRelH

    -- Rendering parameters.
    -- Note that the annotations are parsed in a separate step.

    let bRenParams = pure Pm.def {Pm.temporaryDirectory = tmpDir}
            <**> ((\x -> \p -> p {Pm.baseDirectory = x}) <$> bBaseDir)
            <**> ((\x -> \p -> p {Pm.roadWidth = x}) <$> bRoadW)
            <**> ((\x -> \p -> p {Pm.bridgeHeight = x}) <$> bBridgeH)
            <**> ((\x -> \p -> p {Pm.bridgeRelativeWidth = x}) <$> bBridgeRelW)
            <**> ((\x -> \p -> p {Pm.bankingRelativeHeight = x})
                <$> bBankingRelH)
            <**> ((\x -> \p -> p {Pm.transparentBg = x}) <$> bTransparentBg)
            <**> ((\x -> \p -> p {Pm.twoToneTerrain = x}) <$> bTwoToneTerrain)
            <**> ((\x -> \p -> p {Pm.pixelsPerTile = x}) <$> bPxPtPerTile)
            <**> ((\x -> \p -> p {Pm.xTileBounds = x}) <$> bBoundsX)
            <**> ((\x -> \p -> p {Pm.yTileBounds = x}) <$> bBoundsY)
            <**> ((\x -> \p -> p {Pm.drawGridLines = x}) <$> bDrawGrid)
            <**> ((\x -> \p -> p {Pm.drawIndices = x}) <$> bDrawIndices)
            <**> ((\x -> \p -> p {Pm.outputType = x}) <$> bOutType)

    -- The go button.

    btnGo <- UI.button #. "go-button" #+ [string "Draw map"]
    let eGo = UI.click btnGo

    -- Log handling

    (eAppendToLog, appendToLog) <- liftIO newEvent
    (eStringToLog, stringToLog) <- liftIO newEvent

    let appendLnTo = flip mappend . flip mappend (Pm.logFromList "\r\n")
        ePutLnLog = concatE . map (appendLnTo <$>) $
            [ eAppendToLog, Pm.logFromList <$> eStringToLog ]

        -- The log is cleared at the beginning of the chain.
        eClearLog = const mempty <$ eGo

    -- Note that if an eClearLog is simultaneous with an ePutLnLog the line
    -- will not be appended.
    bLogContents <- mempty `accumB` (eClearLog `union` ePutLnLog)

    -- Displaying the whole log at once, at the end.
    -- We will have to reconsider should the log have to be used before
    -- the start of the chain.
    txaLog <-
        UI.textarea # set UI.id_ "log-text"
            # set UI.cols "72" # set UI.rows "6"
            # set value
                (maybe "" (("Program version: " ++) . (++ "\r\n")) versionString)

    (eRenderLog, renderLog) <- liftIO newEvent
    onEvent (bLogContents <@ eRenderLog) $
        (element txaLog #) . set value . Pm.logToList

    -- Misc. interesting elements

    lnkTrk <-
        UI.a # set UI.id_ "save-trk-link" # set UI.target "_blank" #+
            [string "track"]
    lnkTerrTrk <-
        UI.a # set UI.id_ "save-terrain-link" # set UI.target "_blank" #+
            [string "terrain"]
    lnkFlipbook <-
        UI.a # set UI.id_ "save-flipbook" # set UI.target "_blank" #+
            [string "flipbook"]

    txaAnns <-
        UI.textarea # set UI.name "ann-input"
            # set UI.id_ "ann-input"
            # set UI.cols "25" # set UI.rows "5"

    txaFlipbook <-
        UI.textarea # set UI.name "flipbook-input"
            # set UI.id_ "flipbook-input"
            # set UI.cols "25" # set UI.rows "5"

    imgMap <- UI.img # set UI.id_ "track-map" # set UI.src "static/images/welcome.png"

    -- Assembling the interface HTML.

    theBody <- getBody w # set UI.id_ "the-body" #. "blank-horizon" #+
        [ UI.div # set UI.id_ "left-bar" #+
            [ UI.p #+
                [ element btnGo, string " as ", element selOutput ]
            , UI.p #+
                [ string "Base path:", UI.br
                , element itxBasePath, UI.br
                , string "TRK / RPL relative path:", UI.br
                , element itxTrkPath
                ]
            , UI.p #+
                [ string "Save: "
                , element lnkTrk, string " - "
                , element lnkTerrTrk, string " - "
                , element lnkFlipbook
                ]
            , UI.p #+
                [ string "Style presets:", UI.br
                , element selPreset, element btnPreset, UI.br
                , string "Road width:", UI.br
                , element bidRoadW, UI.br
                , string "Bridge height:", UI.br
                , element bidBridgeH, UI.br
                , string "Bridge relative width:", UI.br
                , element bidBridgeRelW, UI.br
                , string "Banking relative height:", UI.br
                , element bidBankingRelH
                ]
            , UI.p #+
                [ string "Two-tone terrain?", element chkTwoToneTerrain, UI.br
                , string "Transparent low ground?", element chkTransparentBg
                ]
            , UI.p #+
                [ string "Grid?", element chkDrawGrid
                , string " Indices?", element chkDrawIndices, UI.br
                , string "Map bounds (0 - 29):", UI.br
                , row
                    [ string "x from ", element biiMinX
                    , string " to ", element biiMaxX
                    ]
                , row
                    [ string "y from ", element biiMinY
                    , string " to ", element biiMaxY
                    ]
                , UI.br
                , element strPxPtPerTile, UI.br
                , element bidPxPtPerTile, UI.br
                ]
            , UI.p #+
                [ string "Annotations ("
                , UI.a # set UI.text "help"
                    # set UI.href ("static/annotations-help.html")
                    # set UI.target "_blank"
                , string ")", UI.br
                , element txaAnns
                ]
            , UI.p #+
                [ string "Flipbook ("
                , UI.a # set UI.text "help"
                    # set UI.href ("static/annotations-help.html#creating-flipbooks")
                    # set UI.target "_blank"
                , string ")", UI.br
                , element txaFlipbook
                ]
            ]
        , UI.div # set UI.id_ "main-wrap" #+
            [ element imgMap
            , UI.p #+
                [ string "Log:", UI.br
                , element txaLog
                ]
            ]
        ]

    -- The main action proper.

    let runRenderMap :: Pm.RenderingParameters -> Pm.RenderingState
                     -> UI (Pm.RenderingState, Pm.RenderingLog)
        runRenderMap params st = do

            element btnGo # set UI.enabled False

            trkRelPath <- itxTrkPath # get value
            let basePath = Pm.baseDirectory params
                trkPath = basePath </> trkRelPath

            outcome <- runExceptT $ do

                trkExists <- liftIO $ doesFileExist trkPath
                unless trkExists . void $
                    throwError "File does not exist."

                let fileExt = map toUpper $ takeExtension trkPath
                    extIsKnown = fileExt == ".TRK" || fileExt == ".RPL"
                unless extIsKnown . void $
                    throwError "Bad file extension: should be .TRK or .RPL, in upper or lower case."

                fileSize <- liftIO $ getFileSize trkPath
                let sizeIsCorrect = fileSize >= 1802 && fileSize <= 13802
                    badTRKSize = fileExt == ".TRK" && not sizeIsCorrect
                when badTRKSize . void $
                    throwError "Bad file size: .TRK files must have between 1802 and 13802 bytes."

                -- Decide on input format.
                let imgWriter :: FilePath -> CartoT (ExceptT String UI) Pm.PostRenderInfo
                    imgWriter = case fileExt of
                        ".TRK" -> writeImageFromTrk
                        ".RPL" -> writeImageFromRpl
                        _      -> error "Unrecognized input extension."

                -- Parse annotations and render the map.
                let goCarto :: CartoT (ExceptT String UI) Pm.PostRenderInfo
                    goCarto = do
                        anns <- (lift . lift $ txaAnns # get value)
                            >>= parseAnnotations
                        fbks <- (lift . lift $ txaFlipbook # get value)
                            >>= parseFlipbook
                        -- For some reason this is showing up at the end of the rendering.
                        lift . lift $ unless (null fbks) $ alertifyLog'
                            "Flipbook rendering usually takes a few minutes. Please stand by..."
                            StandardLog 10000
                        lift . lift $ flushCallBuffer
                        RWS.local (\p -> p
                            { Pm.annotationSpecs = anns
                            , Pm.flipbookSpec = fbks
                            }) $ imgWriter trkPath
                (postRender,st',logW) <- RWS.runRWST goCarto params st

                -- Update the UI.
                lift $ do
                    when (isJust $ Pm.flipbookRelPath postRender) $ alertifySuccess
                        "Flipbook ready! Use the flipbook link on the left to save it."
                    element theBody #. horizonClass (Pm.renderedTrackHorizon postRender)
                    let outType = Pm.outputType params
                    trackImage <- loadTrackImage outType
                        (tmpDir </> Pm.outputRelPath postRender)
                    trkUri <- loadTmpTrk tmpDir postRender
                    terrainUri <- loadTmpTerrainTrk tmpDir postRender
                    mFlipbookUri <- maybe (return Nothing)
                        ((Just <$>) . loadFile "application/zip" . (tmpDir </>))
                        (Pm.flipbookRelPath postRender)
                    element imgMap # set UI.src trackImage
                    element lnkTrk # set UI.href trkUri
                    element lnkTerrTrk # set UI.href terrainUri
                    maybe (element lnkFlipbook # unsetHref)
                        ((element lnkFlipbook #) . set UI.href) mFlipbookUri

                    return (st', logW)

                `catchError` \errorMsg -> do

                    lift $ do
                        element theBody #. "blank-horizon"
                        element imgMap # set UI.src "static/images/failure.png"
                        mapM_ (unsetHref . element) [lnkTrk, lnkTerrTrk, lnkFlipbook]

                    throwError errorMsg

            element btnGo # set UI.enabled True

            return $ either ((,) st . Pm.logFromList) id outcome


    -- Collecting the parameters and firing the main action.

    mdo
        let eRenParams = bRenParams <@ eGo

        -- Output from the main action, input for the next run.
        (eRenState, fireRenState) <- liftIO newEvent
        bRenState <- Pm.def `stepper` eRenState

        let (eRenParamsDiffEStyle, eRenParamsSameEStyle) = split $
                (\es -> \p -> if Pm.toElemStyle p /= es
                    then Left p else Right p) <$> bRenEStyle <@> eRenParams

            -- The immediate trigger of the main action.
            eParamsAndStateAfterEStyleCheck =
                (flip (,) . Pm.clearTerrainCache . Pm.clearElementCache
                    <$> bRenState
                    <@> eRenParamsDiffEStyle)
                `union` (flip (,) <$> bRenState
                    <@> eRenParamsSameEStyle)

            eRenEStyle = Pm.toElemStyle . fst
                <$> eParamsAndStateAfterEStyleCheck

        -- Element style used in the *previous* run.
        -- Worth pointing out that we have no reason to care what
        -- it is before the first rendering.
        bRenEStyle <- Pm.toElemStyle Pm.def `stepper` eRenEStyle

        -- Firing the main action.

        -- TODO: Ensure it is okay to run appendToLog and then renderLog
        -- like this.
        onEvent eParamsAndStateAfterEStyleCheck $
            (\(p, st) -> runRenderMap p st
                >>= \(st', w) -> liftIO $ fireRenState st'
                    >> appendToLog w >> renderLog ())

removeAttr :: String -> UI Element -> UI Element
removeAttr name el = do
    x <- el
    runFunction $ ffi "$(%1).removeAttr(%2)" x name
    el

unsetHref :: UI Element -> UI Element
unsetHref = removeAttr "href"

loadTmpTrkBase :: (String -> String) -> (LB.ByteString -> LB.ByteString)
               -> FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTrkBase fName fTrk tmpDir postRender = do
    let trkName = "_" ++ fName (Pm.trackName postRender)
        tmpTrkPath = addExtension (tmpDir </> trkName) ".TRK"
    liftIO $ LB.writeFile tmpTrkPath . fTrk $ Pm.trackData postRender
    loadFile "application/octet-stream" tmpTrkPath

loadTmpTrk :: FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTrk = loadTmpTrkBase id id

loadTmpTerrainTrk :: FilePath -> Pm.PostRenderInfo -> UI String
loadTmpTerrainTrk =
    loadTmpTrkBase ((++ "-T") . take 6) terrainTrkSimple

horizonClass :: Horizon -> String
horizonClass horizon = case horizon of
    Desert   -> "desert-horizon"
    Alpine   -> "alpine-horizon"
    City     -> "city-horizon"
    Country  -> "country-horizon"
    Tropical -> "tropical-horizon"
    _        -> "unknown-horizon"

-- A function with this signature used to be necessary to set up the
-- correct MIME type before calling loadFile from Threepenny 0.5.
-- loadTrackImage :: OutputType -> FilePath -> UI String
loadTrackImage :: OutputType -> FilePath -> UI String
loadTrackImage outType outPath = case outType of
    PNG -> loadFile "image/png" outPath
    SVG -> loadFile "image/svg+xml" outPath
    _   -> error "Unsupported output format."

intToOutputType :: Int -> OutputType
intToOutputType n = case n of
        0 -> PNG
        1 -> SVG
        _ -> error "Unknown output format."

-- The order in the case statement matches that in the style-preset-select.
intToPresetRenderingParams :: Int -> Pm.RenderingParameters
intToPresetRenderingParams n = case n of
    0 -> Pm.defaultRenderingParameters
    1 -> Pm.widerRoadsRenderingParameters
    2 -> Pm.slopingRampsRenderingParameters
    3 -> Pm.classicRenderingParameters
    _ -> error "Unknown preset."

-- Other unused utility functions we do not want to throw away (yet).
{-
setTrackMapVisibility :: Bool -> JSFunction ()
setTrackMapVisibility visible
    | visible   = ffi "document.getElementById('track-map').style.display='block';"
    | otherwise = ffi "document.getElementById('track-map').style.display='none';"
-}

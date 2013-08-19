module Main
    ( main
    ) where

import Control.Monad
import qualified Control.Monad.RWS as RWS
import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory ( doesFileExist, doesDirectoryExist
                        , getTemporaryDirectory)
import System.FilePath ((</>), takeExtension, addExtension)
import System.CPUTime
import Data.Char (toUpper)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Diagrams.Backend.Cairo (OutputType(..))

import Output
import qualified OurByteString as LB
import Track (Horizon(..), terrainTrkSimple)
import qualified Parameters as Pm
import Utils (retrieveFileSize)
import Annotate (Annotation)
import AnnotationParser (parseAnnotations)

main :: IO ()
main = do
    startGUI Config
        { tpPort = 10000
        , tpCustomHTML = Nothing
        , tpStatic = "."
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Stunts Cartography - Track Viewer"
    UI.addStyleSheet w "viewer.css"
    getBody w # set UI.id_ "the-body" # set UI.class_ "blank-horizon" #+
        [ UI.div # set UI.id_ "left-bar" #+
            [ UI.p #+
                [ mkButtonGo
                , string " as "
                , UI.select # set UI.name "output-format-select"
                    # set UI.id_ "output-format-select" #+
                    [ UI.option # set UI.selected True #+ [string "PNG"]
                    , UI.option #+ [string "SVG"]
                    ]
                ]
            , UI.p #+
                [ string "Base path:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "base-path-input"
                    # set UI.id_ "base-path-input" # set value ".."
                ]
            , UI.p #+
                [ string "TRK / RPL relative path:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "trk-input"
                    # set UI.id_ "trk-input"
                ]
            , UI.p #+
                [ string "Save as: "
                , UI.a # set UI.id_ "save-trk-link" # set UI.target "_blank" #+
                    [string "track"]
                , string " - "
                , UI.a # set UI.id_ "save-terrain-link" # set UI.target "_blank" #+
                    [string "terrain"]
                ]
            , UI.p #+
                [ string "Style presets:"
                , UI.br
                , UI.select # set UI.name "style-preset-select"
                    # set UI.id_ "style-preset-select" #+
                    [ UI.option # set UI.selected True #+ [string "Default"]
                    , UI.option #+ [string "Wider track"]
                    , UI.option #+ [string "Sloping ramps"]
                    , UI.option #+ [string "Traditional"]
                    ]
                , mkButtonApplyPreset
                ]
            , UI.p #+
                [ string "Road width:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "road-w-input"
                    # set UI.id_ "road-w-input" # set UI.size "5"
                , string " from 0.1 to 0.5"
                ]
            , UI.p #+
                [ string "Bridge height:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "bridge-h-input"
                    # set UI.id_ "bridge-h-input" # set UI.size "5"
                , string " from 0 to 0.5"
                ]
            , UI.p #+
                [ string "Bridge relative width:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "bridge-rel-w-input"
                    # set UI.id_ "bridge-rel-w-input" # set UI.size "5"
                , string " from 1 to 3"
                ]
            , UI.p #+
                [ string "Banking relative height:"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "bank-rel-h-input"
                    # set UI.id_ "bank-rel-h-input" # set UI.size "5"
                , string " from 0.25 to 1"
                ]
            , UI.p #+
                [ string "Pixels per tile (PNG)"
                , UI.br
                , string "Points per tile (SVG):"
                , UI.br
                , UI.input # set UI.type_ "text" # set UI.name "px-per-tile-input"
                    # set UI.id_ "px-per-tile-input" # set UI.size "5"
                    # set value "32"
                , string " from 8 to 128"
                ]
            , UI.p #+
                [ string "Grid?"
                , UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
                    # set UI.id_ "grid-lines-chk" # set UI.checked_ True
                , string " Indices?"
                , UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
                    # set UI.id_ "grid-indices-chk" # set UI.checked_ True
                ]
            , UI.p #+
                [ string "Map bounds (0 - 29):"
                , UI.br
                , string "x from "
                , UI.input # set UI.type_ "text"
                    # set UI.name "x-min-bound-input" # set UI.size "2"
                    # set UI.id_ "x-min-bound-input" # set value "0"
                , string " to "
                , UI.input # set UI.type_ "text"
                    # set UI.name "x-max-bound-input" # set UI.size "2"
                    # set UI.id_ "x-max-bound-input" # set value "29"
                , UI.br
                , string "y from "
                , UI.input # set UI.type_ "text"
                    # set UI.name "y-min-bound-input" # set UI.size "2"
                    # set UI.id_ "y-min-bound-input" # set value "0"
                , string " to "
                , UI.input # set UI.type_ "text" # set UI.size "2"
                    # set UI.name "y-max-bound-input"
                    # set UI.id_ "y-max-bound-input" # set value "29"
                ]
            , UI.p #+
                [ string "Annotations:" -- TODO: Add help.
                , UI.br
                , UI.textarea # set UI.name "ann-input"
                    # set UI.id_ "ann-input"
                    # set UI.cols "25" # set UI.rows "5"
                ]
            ]
        , UI.div # set UI.id_ "main-wrap" #+
            [ UI.img # set UI.id_ "track-map"
            , UI.p #+
                [ string "Log:"
                , UI.br
                , UI.textarea # set UI.id_ "log-text"
                    # set UI.cols "72" # set UI.rows "6"
                ]
            ]
        ]
    fillDrawingRatiosFields w Pm.defaultRenderingParameters

clearLog :: Window -> IO Element
clearLog w = set value "" $ fromJust <$> getElementById w "log-text"

appendLineToLog :: Window -> String -> IO ()
appendLineToLog w msg = do
    logEl <- fromJust <$> getElementById w "log-text"
    msgs <- get value logEl
    set value (msgs ++ msg ++ "\r\n") $ return logEl
    return ()

formatTiming :: Double -> String
formatTiming = printf "Total rendering time: %0.4f seconds."

-- TODO: Yuck.
currentRenderingState :: IORef (Pm.RenderingParameters, Pm.RenderingState)
{-# NOINLINE currentRenderingState #-}
currentRenderingState = unsafePerformIO $ newIORef
    (Pm.defaultRenderingParameters, Pm.initialRenderingState)

mkButtonGo :: IO Element
mkButtonGo = do
    button <- UI.button #. "go-button" #+ [string "Draw map"]
    on UI.click button $ generateImageHandler button
    return button

generateImageHandler :: Element -> (a -> IO ())
generateImageHandler button = \_ -> do
    w <- fromJust <$> getWindow button
    clearLog w
    trkRelPath <- join $ get value . fromJust
        <$> getElementById w "trk-input"
    basePath <- join $ get value . fromJust
        <$> getElementById w "base-path-input"
    let trkPath = basePath </> trkRelPath
    trkExists <- doesFileExist trkPath
    mFileSize <- retrieveFileSize trkPath
    let sizeIsCorrect = mFileSize == Just 1802
        fileExt = map toUpper $ takeExtension trkPath
        extIsKnown = fileExt == ".TRK" || fileExt == ".RPL"
        -- No size checks for .RPL at this point.
        badTRKSize = fileExt == ".TRK" && not sizeIsCorrect
        proceedWithLoading = trkExists && extIsKnown && not badTRKSize
    if proceedWithLoading
        then do
            let pngWriter = case fileExt of
                    ".TRK" -> writePngFromTrk
                    ".RPL" -> writePngFromRpl
                    _      -> error "Unrecognized input extension."
            outType <- selectedOutputFormat w
            params <- selectedRenderingParameters w outType
            (oldParams, st) <- readIORef currentRenderingState
            st' <- if params `Pm.elementStyleIsDifferent` oldParams
                then do
                    let s' = Pm.clearElementCache st
                    atomicModifyIORef' currentRenderingState (\(p, _) ->
                        ((p, s'), s'))
                else return st
            startTime <- getCPUTime
            -- TODO: Actually use the Writer output.
            (postRender,st'',_) <- RWS.runRWST (pngWriter trkPath) params st'
            endTime <- getCPUTime
            let deltaTime = fromIntegral (endTime - startTime) / 10^12
            appendLineToLog w $ formatTiming deltaTime
            atomicWriteIORef currentRenderingState (params, st'')

            applyHorizonClass w $ Pm.renderedTrackHorizon postRender
            trackImage <- loadTrackImage w outType $ Pm.outputPath postRender
            trkUri <- loadTmpTrk w postRender
            terrainUri <- loadTmpTerrainTrk w postRender
            runFunction w $ setTrackMapVisibility True
            (fromJust <$> getElementById w "track-map") # set UI.src trackImage
            setLinkHref w "save-trk-link" trkUri
            setLinkHref w "save-terrain-link" terrainUri
            return ()
        else do
            unless (extIsKnown || not trkExists) $
                appendLineToLog w
                    "Bad file extension (should be .TRK or .RPL, in upper or lower case)."
            unless trkExists $
                appendLineToLog w "File does not exist."
            when (badTRKSize && trkExists) $
                appendLineToLog w "Bad file size (.TRK files must have 1802 bytes)."

            applyClassToBody w "blank-horizon"
            runFunction w $ setTrackMapVisibility False
            runFunction w $ unsetSaveLinksHref

setTrackMapVisibility :: Bool -> JSFunction ()
setTrackMapVisibility visible
    | visible   = ffi "document.getElementById('track-map').style.display='block';"
    | otherwise = ffi "document.getElementById('track-map').style.display='none';"

setLinkHref :: Window -> String -> String -> IO Element
setLinkHref w linkId uri =
    (fromJust <$> getElementById w linkId) # set UI.href uri

unsetSaveLinksHref :: JSFunction ()
unsetSaveLinksHref = ffi $ unlines
    [ "document.getElementById('save-trk-link').removeAttribute('href');"
    , "document.getElementById('save-terrain-link').removeAttribute('href');"
    ]

loadTmpTrkBase :: (String -> String) -> (LB.ByteString -> LB.ByteString)
               -> Window -> Pm.PostRenderInfo -> IO String
loadTmpTrkBase fName fTrk w postRender = do
    tmpDir <- getTemporaryDirectory
        `catch` ((\_ -> return ".") :: SomeException -> IO String)
    let trkName = "_" ++ fName (Pm.trackName postRender)
        tmpTrkPath = addExtension (tmpDir </> trkName) ".TRK"
    LB.writeFile tmpTrkPath . fTrk $ Pm.trackData postRender
    loadFile w "application/octet-stream" tmpTrkPath

loadTmpTrk :: Window -> Pm.PostRenderInfo -> IO String
loadTmpTrk = loadTmpTrkBase id id

loadTmpTerrainTrk :: Window -> Pm.PostRenderInfo -> IO String
loadTmpTerrainTrk =
    loadTmpTrkBase ((++ "-T") . take 6) terrainTrkSimple

applyHorizonClass :: Window -> Horizon -> IO ()
applyHorizonClass w horizon = do
    let horizonClass = case horizon of
            Desert   -> "desert-horizon"
            Alpine   -> "alpine-horizon"
            City     -> "city-horizon"
            Country  -> "country-horizon"
            Tropical -> "tropical-horizon"
            _        -> "unknown-horizon"
    applyClassToBody w horizonClass

applyClassToBody :: Window -> String -> IO ()
applyClassToBody w klass = getBody w # set UI.class_ klass >> return ()

loadTrackImage :: Window -> OutputType -> FilePath -> IO String
loadTrackImage w outType outPath = case outType of
    PNG -> loadFile w "image/png" outPath
    SVG -> loadFile w "image/svg+xml" outPath
    _   -> error "Unsupported output format."

selectedRenderingParameters :: Window -> OutputType -> IO Pm.RenderingParameters
selectedRenderingParameters w outType = do
    roadW <- selectedRoadWidth w
    bridgeH <- selectedBridgeHeight w
    bridgeRelW <- selectedBridgeRelativeWidth w
    bankRelH <- selectedBankingRelativeHeight w
    pxPerTile <- selectedPixelsPerTile w
    drawGrid <- selectedDrawGridLines w
    drawIxs <- selectedDrawGridIndices w
    xBounds <- selectedXTileBounds w
    yBounds <- selectedYTileBounds w
    annSpecs <- selectedAnnotations w
    return Pm.defaultRenderingParameters
            { Pm.roadWidth = roadW
            , Pm.bridgeHeight = bridgeH
            , Pm.bridgeRelativeWidth = bridgeRelW
            , Pm.bankingRelativeHeight = bankRelH
            , Pm.pixelsPerTile = pxPerTile
            , Pm.drawGridLines = drawGrid
            , Pm.drawIndices = drawIxs
            , Pm.outputType = outType
            , Pm.xTileBounds = xBounds
            , Pm.yTileBounds = yBounds
            , Pm.annotationSpecs = annSpecs
            }

selectedFromSelect :: (Int -> a) -> String -> Window -> IO a
selectedFromSelect fSel selId = \w -> do
    optionIx <- join $ liftM (fromMaybe 0) . get UI.selection . fromJust
        <$> getElementById w selId
    return $ fSel optionIx

selectedOutputFormat :: Window -> IO OutputType
selectedOutputFormat = selectedFromSelect fSel "output-format-select"
    where
    fSel = \n -> case n of
        0 -> PNG
        1 -> SVG
        _ -> error "Unknown output format."

mkButtonApplyPreset :: IO Element
mkButtonApplyPreset = do
    button <- UI.button #. "button" #+ [string "Set"]
    on UI.click button $ applyPresetHandler button
    return button

applyPresetHandler :: Element -> (a -> IO ())
applyPresetHandler button = \_ -> do
    w <- fromJust <$> getWindow button
    preset <- selectedPresetRenderingParams w
    fillDrawingRatiosFields w preset

fillDrawingRatiosFields :: Window -> Pm.RenderingParameters -> IO ()
fillDrawingRatiosFields w params = do
    let txtValues = map (printf "%.3f" . ($ params)) txtFuncs
    txtElems <- map return <$> getElementsById w txtElIds
    sequence_ $ zipWith (set value) txtValues txtElems
    where
    txtFuncs :: [Pm.RenderingParameters -> Double]
    txtFuncs = map (realToFrac .)
        [ Pm.roadWidth
        , Pm.bridgeHeight
        , Pm.bridgeRelativeWidth
        , Pm.bankingRelativeHeight
        ]
    txtElIds =
        [ "road-w-input"
        , "bridge-h-input"
        , "bridge-rel-w-input"
        , "bank-rel-h-input"
        ]

-- The order in the case statement matches that in the style-preset-select.
selectedPresetRenderingParams :: Window -> IO Pm.RenderingParameters
selectedPresetRenderingParams = selectedFromSelect fSel "style-preset-select"
    where
    fSel = \n -> case n of
        0 -> Pm.defaultRenderingParameters
        1 -> Pm.widerRoadsRenderingParameters
        2 -> Pm.slopingRampsRenderingParameters
        3 -> Pm.classicRenderingParameters
        _ -> error "Unknown preset."

selectedNumFromTextInput :: (Num a, Read a, Ord a)
                         => String -> a -> a -> a
                         -> Window -> IO a
selectedNumFromTextInput elemId minVal defVal maxVal = \w -> do
    inputStr <- join $ get value . fromJust
        <$> getElementById w elemId
    let val = fromMaybe defVal . readMaybe $ inputStr
    return $ min maxVal . max minVal $ val

selectedRoadWidth :: Window -> IO Double
selectedRoadWidth =
    selectedNumFromTextInput "road-w-input" 0.1 0.2 0.5

selectedBridgeHeight :: Window -> IO Double
selectedBridgeHeight = do
    selectedNumFromTextInput "bridge-h-input" 0 0 0.5

selectedBridgeRelativeWidth :: Window -> IO Double
selectedBridgeRelativeWidth = do
    selectedNumFromTextInput "bridge-rel-w-input" 1 2 3

selectedBankingRelativeHeight :: Window -> IO Double
selectedBankingRelativeHeight = do
    selectedNumFromTextInput "bank-rel-h-input" 0.25 0.5 1

selectedPixelsPerTile :: Window -> IO Double
selectedPixelsPerTile =
    selectedNumFromTextInput "px-per-tile-input" 8 32 128

selectedBoolFromCheckbox :: String -> Window -> IO Bool
selectedBoolFromCheckbox elemId = \w ->
    join $ get UI.checked . fromJust <$> getElementById w elemId

selectedDrawGridLines :: Window -> IO Bool
selectedDrawGridLines = selectedBoolFromCheckbox "grid-lines-chk"

selectedDrawGridIndices :: Window -> IO Bool
selectedDrawGridIndices = selectedBoolFromCheckbox "grid-indices-chk"

selectedXTileBounds :: Window -> IO (Int, Int)
selectedXTileBounds w = liftM ensureBoundOrder $ (,) <$>
    (selectedNumFromTextInput "x-min-bound-input" 0 0 29 w)
    <*> (selectedNumFromTextInput "x-max-bound-input" 0 29 29 w)

selectedYTileBounds :: Window -> IO (Int, Int)
selectedYTileBounds w = liftM ensureBoundOrder $ (,) <$>
    (selectedNumFromTextInput "y-min-bound-input" 0 0 29 w)
    <*> (selectedNumFromTextInput "y-max-bound-input" 0 29 29 w)

ensureBoundOrder :: (Int, Int) -> (Int, Int)
ensureBoundOrder bounds@(z, w) = if z > w then (w, z) else bounds

selectedAnnotations :: Window -> IO [Annotation]
selectedAnnotations w = do
    annString <- join $ get value . fromJust
        <$> getElementById w "ann-input"
    return $ parseAnnotations annString

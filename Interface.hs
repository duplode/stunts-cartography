module Main
    ( main
    ) where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch, SomeException)
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory ( doesFileExist, doesDirectoryExist
                        , getTemporaryDirectory)
import System.FilePath ((</>), takeExtension, addExtension)
import Data.Char (toUpper)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Diagrams.Backend.Cairo (OutputType(..))

import Output
import qualified OurByteString as LB
import Track (Horizon(..))
import qualified Parameters as Pm
import Utils (retrieveFileSize)
import Replay (terrainTrkSimple)

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
            [ UI.p #+ [string "Generate image:"]
            , mkButtonGo
            , mkButtonSVG
            , UI.p #+ [string "Base path:"]
            , UI.input # set UI.type_ "text" # set UI.name "base-path-input"
                # set UI.id_ "base-path-input" # set value ".."
            , UI.p #+ [string ".TRK or .RPL relative path:"]
            , UI.input # set UI.type_ "text" # set UI.name "trk-input"
                # set UI.id_ "trk-input"
            , UI.p #+
                [ string "Save as: "
                , UI.a # set UI.id_ "save-trk-link" # set UI.target "_blank" #+
                    [string "track"]
                , string " - "
                , UI.a # set UI.id_ "save-terrain-link" # set UI.target "_blank" #+
                    [string "terrain"]
                ]
            , UI.p #+ [string "Style presets:"]
            , UI.p #+
                [ UI.select # set UI.name "style-preset-select"
                    # set UI.id_ "style-preset-select" #+
                    [ UI.option # set UI.selected True #+ [string "To scale (default)"]
                    , UI.option #+ [string "Wider track"]
                    , UI.option #+ [string "Sloping ramps"]
                    , UI.option #+ [string "Traditional"]
                    ]
                , mkButtonApplyPreset
                ]
            , UI.p #+ [string "Road width (0.1 - 0.5):"]
            , UI.input # set UI.type_ "text" # set UI.name "road-w-input"
                # set UI.id_ "road-w-input" # set value "0.2"
            , UI.p #+ [string "Bridge height (0 - 0.5):"]
            , UI.input # set UI.type_ "text" # set UI.name "bridge-h-input"
                # set UI.id_ "bridge-h-input" # set value "0"
            , UI.p #+ [string "Bridge relative width (1 - 3):"]
            , UI.input # set UI.type_ "text" # set UI.name "bridge-rel-w-input"
                # set UI.id_ "bridge-rel-w-input" # set value "2"
            , UI.p #+ [string "Banking relative height (0.25 - 1):"]
            , UI.input # set UI.type_ "text" # set UI.name "bank-rel-h-input"
                # set UI.id_ "bank-rel-h-input" # set value "0.5"
            , UI.p #+ [string "Pixels (PNG) or points (SVG) per tile (8 - 64):"]
            , UI.input # set UI.type_ "text" # set UI.name "px-per-tile-input"
                # set UI.id_ "px-per-tile-input" # set value "32"
            , UI.p #+
                [ string "Grid lines?"
                , UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
                    # set UI.id_ "grid-lines-chk" # set UI.checked_ True
                , string " indices?"
                , UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
                    # set UI.id_ "grid-indices-chk" # set UI.checked_ True
                ]
            , UI.p #+ [string "Map bounds (0 - 29):"]
            , UI.p #+
                [ string "x from "
                , UI.input # set UI.type_ "text"
                    # set UI.name "x-min-bound-input" # set UI.size "2"
                    # set UI.id_ "x-min-bound-input" # set value "0"
                , string " to "
                , UI.input # set UI.type_ "text"
                    # set UI.name "x-max-bound-input" # set UI.size "2"
                    # set UI.id_ "x-max-bound-input" # set value "29"
                ]
            , UI.p #+
                [ string "y from "
                , UI.input # set UI.type_ "text"
                    # set UI.name "y-min-bound-input" # set UI.size "2"
                    # set UI.id_ "y-min-bound-input" # set value "0"
                , string " to "
                , UI.input # set UI.type_ "text" # set UI.size "2"
                    # set UI.name "y-max-bound-input"
                    # set UI.id_ "y-max-bound-input" # set value "29"
                ]
            ]
        , UI.div # set UI.id_ "main-wrap" #+
            [ UI.img # set UI.id_ "track-map"]
        ]

mkButtonGo :: IO Element
mkButtonGo = do
    button <- UI.button #. "button" #+ [string "PNG"]
    on UI.click button $ generateImageHandler PNG button
    return button

mkButtonSVG :: IO Element
mkButtonSVG = do
    button <- UI.button #. "button" #+ [string "SVG"]
    on UI.click button $ generateImageHandler SVG button
    return button

generateImageHandler :: OutputType -> Element -> (a -> IO ())
generateImageHandler outType button = \_ -> do
    w <- fromJust <$> getWindow button --TODO: ick
    trkRelPath <- join $ get value . fromJust
        <$> getElementById w "trk-input"
    basePath <- join $ get value . fromJust
        <$> getElementById w "base-path-input"
    let trkPath = basePath </> trkRelPath
    trkExists <- doesFileExist trkPath
    mFileSize <- retrieveFileSize trkPath
    let sizeIsCorrect = mFileSize == Just 1802
        fileExt = map toUpper $ takeExtension trkPath
        proceedWithLoading = trkExists
            && ((fileExt == ".TRK" && sizeIsCorrect)
                || fileExt == ".RPL" -- No size checks at this point.
                )
    if proceedWithLoading
        then do
            let pngWriter = case fileExt of
                    ".TRK" -> writePngFromTrk
                    ".RPL" -> writePngFromRpl
                    _      -> error "Unrecognized input extension."
            params <- selectedRenderingParameters w outType
            postRender <- pngWriter params trkPath
            applyHorizonClass w $ Pm.renderedTrackHorizon postRender
            trackImage <- loadTrackImage w outType $ Pm.outputPath postRender
            trkUri <- loadTmpTrk w postRender
            terrainUri <- loadTmpTerrainTrk w postRender
            (fromJust <$> getElementById w "track-map") # set UI.src trackImage
            setLinkHref w "save-trk-link" trkUri
            setLinkHref w "save-terrain-link" terrainUri
            return ()
        else do
            applyClassToBody w "blank-horizon"
            (fromJust <$> getElementById w "track-map") # set UI.src ""
            runFunction w $ unsetSaveLinksHref

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
            }

mkButtonApplyPreset :: IO Element
mkButtonApplyPreset = do
    button <- UI.button #. "button" #+ [string "Apply"]
    on UI.click button $ applyPresetHandler button
    return button

applyPresetHandler :: Element -> (a -> IO ())
applyPresetHandler button = \_ -> do
    w <- fromJust <$> getWindow button
    preset <- selectedPresetRenderingParams w
    let txtValues = map (printf "%.3f" . ($ preset)) txtFuncs
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
selectedPresetRenderingParams w = do
    paramsSel <- join $ liftM (fromMaybe 0) . get UI.selection . fromJust
        <$> getElementById w "style-preset-select"
    return $ case paramsSel of
        0 -> Pm.defaultRenderingParameters
        1 -> Pm.widerRoadsRenderingParameters
        2 -> Pm.slopingRampsRenderingParameters
        3 -> Pm.classicRenderingParameters

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
    selectedNumFromTextInput "px-per-tile-input" 8 32 64

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

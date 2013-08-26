{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where

import Control.Monad
import qualified Control.Monad.RWS as RWS
import Control.Exception (catch, SomeException)
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory ( doesFileExist, doesDirectoryExist
                        , getTemporaryDirectory)
import System.FilePath ((</>), takeExtension, addExtension)
import Data.Char (toUpper)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event, newEvent, filterJust)
import qualified Graphics.UI.Threepenny.Core as Reg (Event, newEvent)
import Reactive.Banana
import Reactive.Banana.Threepenny
import Diagrams.Backend.Cairo (OutputType(..))

import Output
import qualified Util.ByteString as LB
import Track (Horizon(..), terrainTrkSimple)
import qualified Parameters as Pm
import Util.Misc (retrieveFileSize)
import Annotation (Annotation)
import Annotation.Parser (parseAnnotations)
import Types.CartoM
import Paths
import qualified Widgets.BoundedInput as BI

main :: IO ()
main = do
    staticDir <- (</> "wwwroot") <$> getDataDir
    startGUI Config
        { tpPort = 10000
        , tpCustomHTML = Nothing
        , tpStatic = staticDir
        } setup

setup :: Window -> IO ()
setup w = void $ do

    -- Prolegomena.

    return w # set title "Stunts Cartography - Track Viewer"
    UI.addStyleSheet w "viewer.css"

    -- Bindings for interesting HTML elements.

    btnGo <- UI.button #. "go-button" #+ [string "Draw map"]

    selOutput <-
        UI.select # set UI.name "output-format-select"
            # set UI.id_ "output-format-select" #+
                [ UI.option # set UI.selected True #+ [string "PNG"]
                , UI.option #+ [string "SVG"]
                ]

    itxBasePath <-
        UI.input # set UI.type_ "text" # set UI.name "base-path-input"
            # set UI.id_ "base-path-input" # set value ".."
    itxTrkPath <-
        UI.input # set UI.type_ "text" # set UI.name "trk-input"
            # set UI.id_ "trk-input"

    lnkTrk <-
        UI.a # set UI.id_ "save-trk-link" # set UI.target "_blank" #+
            [string "track"]
    lnkTerrTrk <-
        UI.a # set UI.id_ "save-terrain-link" # set UI.target "_blank" #+
            [string "terrain"]

    selPreset <-
        UI.select # set UI.name "style-preset-select"
            # set UI.id_ "style-preset-select" #+
                [ UI.option # set UI.selected True #+ [string "Default"]
                , UI.option #+ [string "Wider track"]
                , UI.option #+ [string "Sloping ramps"]
                , UI.option #+ [string "Traditional"]
                ]

    btnPreset <- UI.button #. "button" #+ [string "Set"]

    itxRoadW <-
        UI.input # set UI.type_ "text" # set UI.name "road-w-input"
            # set UI.id_ "road-w-input" # set UI.size "5"

    itxBridgeH <-
        UI.input # set UI.type_ "text" # set UI.name "bridge-h-input"
            # set UI.id_ "bridge-h-input" # set UI.size "5"

    itxBridgeRelW <-
        UI.input # set UI.type_ "text" # set UI.name "bridge-rel-w-input"
            # set UI.id_ "bridge-rel-w-input" # set UI.size "5"

    itxBankingRelH <-
        UI.input # set UI.type_ "text" # set UI.name "bank-rel-h-input"
            # set UI.id_ "bank-rel-h-input" # set UI.size "5"

    strPxPtPerTile <- string "Pixels per tile:"
    itxPxPtPerTile <-
        UI.input # set UI.type_ "text" # set UI.name "px-per-tile-input"
            # set UI.id_ "px-per-tile-input" # set UI.size "5"
            # set value "32"

    chkDrawGrid <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
            # set UI.id_ "grid-lines-chk" # set UI.checked_ True
    chkDrawIndices <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
            # set UI.id_ "grid-indices-chk" # set UI.checked_ True

    itxBMinX <-
        UI.input # set UI.type_ "text"
            # set UI.name "x-min-bound-input" # set UI.size "2"
            # set UI.id_ "x-min-bound-input" # set value "0"
    itxBMaxX <-
        UI.input # set UI.type_ "text"
            # set UI.name "x-max-bound-input" # set UI.size "2"
            # set UI.id_ "x-max-bound-input" # set value "29"
    itxBMinY <-
        UI.input # set UI.type_ "text"
            # set UI.name "y-min-bound-input" # set UI.size "2"
            # set UI.id_ "y-min-bound-input" # set value "0"
    itxBMaxY <-
        UI.input # set UI.type_ "text" # set UI.size "2"
            # set UI.name "y-max-bound-input"
            # set UI.id_ "y-max-bound-input" # set value "29"

    txaAnns <-
        UI.textarea # set UI.name "ann-input"
            # set UI.id_ "ann-input"
            # set UI.cols "25" # set UI.rows "5"

    imgMap <- UI.img # set UI.id_ "track-map"

    txaLog <-
        UI.textarea # set UI.id_ "log-text"
            # set UI.cols "72" # set UI.rows "6"

    biTest <- BI.new (0, 30) (15 :: Int)
    elBiTest <- BI.toElement biTest

    -- Assembling the interface HTML.

    getBody w # set UI.id_ "the-body" # set UI.class_ "blank-horizon" #+
        [ UI.div # set UI.id_ "left-bar" #+
            [ UI.p #+
                [ element btnGo, string " as ", element selOutput ]
            , UI.p #+
                [ string "Base path:", UI.br
                , element itxBasePath
                ]
            , UI.p #+
                [ string "TRK / RPL relative path:", UI.br
                , element itxTrkPath
                ]
            , UI.p #+
                [ string "Save as: ", element lnkTrk
                , string " - ", element lnkTerrTrk
                ]
            , UI.p #+
                [ string "Style presets:", UI.br
                , element selPreset, element btnPreset
                ]
            , UI.p #+
                [ string "Road width:", UI.br
                , element itxRoadW, string " from 0.1 to 0.5"
                ]
            , UI.p #+
                [ string "Bridge height:", UI.br
                , element itxBridgeH, string " from 0 to 0.5"
                ]
            , UI.p #+
                [ string "Bridge relative width:", UI.br
                , element itxBridgeRelW, string " from 1 to 3"
                ]
            , UI.p #+
                [ string "Banking relative height:", UI.br
                , element itxBankingRelH, string " from 0.25 to 1"
                ]
            , UI.p #+
                [ element strPxPtPerTile, UI.br
                , element itxPxPtPerTile, string " from 8 to 128"
                ]
            , UI.p #+
                [ string "Grid?", element chkDrawGrid
                , string " Indices?", element chkDrawIndices
                ]
            , UI.p #+
                [ string "Map bounds (0 - 29):", UI.br
                , string "x from ", element itxBMinX
                , string " to ", element itxBMaxX, UI.br
                , string "y from ", element itxBMinY
                , string " to ", element itxBMaxY
                ]
            , UI.p #+
                [ string "Annotations - "
                , UI.a # set UI.text "help"
                    # set UI.href ("static/annotations-help.html")
                    # set UI.target "_blank"
                , UI.br
                , element txaAnns
                ]
            , UI.p #+ [element elBiTest]
            ]
        , UI.div # set UI.id_ "main-wrap" #+
            [ element imgMap
            , UI.p #+
                [ string "Log:", UI.br
                , element txaLog
                ]
            ]
        ]

    -- Initializing fields.

    fillDrawingRatiosFields w Pm.defaultRenderingParameters

    -- The main action proper.

    let runRenderMap :: Pm.RenderingParameters -> Pm.RenderingState
                     -> IO Pm.RenderingState
        runRenderMap params st = do

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
                    -- Decide on input format.
                    let pngWriter = case fileExt of
                            ".TRK" -> writePngFromTrk
                            ".RPL" -> writePngFromRpl
                            _      -> error "Unrecognized input extension."

                    -- Parse annotations and render the map.
                    let goCarto :: CartoT IO Pm.PostRenderInfo
                        goCarto = do
                        anns <- selectedAnnotations w
                        postRender <- RWS.local (\p -> p{ Pm.annotationSpecs = anns}) $
                            pngWriter trkPath
                        return postRender
                    (postRender,st',logW) <- RWS.runRWST goCarto params st

                    -- Update the log.
                    appendLineToLog w $ Pm.logToList logW

                    -- Update the UI.
                    applyHorizonClass w $ Pm.renderedTrackHorizon postRender
                    let outType = Pm.outputType params
                    trackImage <- loadTrackImage w outType $ Pm.outputPath postRender
                    trkUri <- loadTmpTrk w postRender
                    terrainUri <- loadTmpTerrainTrk w postRender
                    runFunction w $ setTrackMapVisibility True
                    (fromJust <$> getElementById w "track-map") # set UI.src trackImage
                    setLinkHref w "save-trk-link" trkUri
                    setLinkHref w "save-terrain-link" terrainUri

                    return st'
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

                    return st

    -- The event network.

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            -- Output type and tile resolution caption

            eOutType <- ((intToOutputType . fromMaybe (-1)) <$>)
                <$> eventSelection selOutput
            let bOutType = intToOutputType 0 `stepper` eOutType

            let ePxPtText =
                    let toPxPtText x =
                            case x of
                                SVG -> "Points per tile:"
                                _   -> "Pixels per tile:" -- PNG
                    in toPxPtText <$> eOutType

            reactimate $
                (\capt -> void $ element strPxPtPerTile
                    # set UI.text capt) <$> ePxPtText

            -- Preset selection.

            ePreset <- ((intToPresetRenderingParams . fromMaybe (-1)) <$>)
                <$> eventSelection selPreset
            let bPreset = intToPresetRenderingParams 0 `stepper` ePreset

            eConfirmPreset <- (bPreset <@)
                <$> event UI.click btnPreset

            reactimate $ (\preset -> fillDrawingRatiosFields w preset)
                <$> eConfirmPreset

            -- The main action, in several parts.

            eBtnGo <- event UI.click btnGo

            -- Rendering parameters.

            (eRenParams, fireRenParams) <- newEvent
            -- For immediate consumption only.

            -- The event fired here indirectly triggers the main action.
            reactimate $
                (\ot -> selectedRenderingParameters w ot
                    >>= fireRenParams) <$> bOutType <@ calm eBtnGo

            -- Output from the main action, input for the next run.

            (eRenState, fireRenState) <- newEvent
            let bRenState = Pm.initialRenderingState `stepper` eRenState

            (eRenEStyle, fireRenEStyle) <- newEvent
            let bRenEStyle = Pm.toElemStyle Pm.defaultRenderingParameters `stepper` eRenEStyle
            -- Worth pointing out that we have no reason to care what
            -- bRenEState is before the first rendering.

            -- An example of how reactimate can make things trickier.  Here, we
            -- cannot query a bRenParams for the updated value of the
            -- parameters when starting the chain with the same eRenParams
            -- which changed the value, as we might end up with the old value.
            -- Rather, we keep the params from the event around.
            let bIsEStyleDifferent = pure (\oldES params ->
                    (params, Pm.toElemStyle params /= oldES)) <*> bRenEStyle

            -- The immediate trigger of the main action.
            let ePreparedCacheState = ((\s (p, f) -> (p, f s)) <$> bRenState) <@>
                    ((\(p, willDo) ->
                        (p, if willDo then Pm.clearElementCache else id))
                            <$> (bIsEStyleDifferent <@> eRenParams))

            -- Firing the main action.

            reactimate $
                (\(p, st) -> runRenderMap p st
                    >>= \st'-> fireRenEStyle (Pm.toElemStyle p) >> fireRenState st')
                        <$> ePreparedCacheState

            -- Debugging.

            eGetDebug <- fromAddHandler (register $ BI.getValueEvent biTest)
            eRequestDebug <- fromAddHandler (register $ UI.click elBiTest)
            reactimate $ BI.requestValue biTest <$ eRequestDebug
            reactimate $
                (\x -> appendLineToLog w (show x))
                    <$> eGetDebug


    compile networkDescription >>= actuate


clearLog :: Window -> IO Element
clearLog w = set value "" $ fromJust <$> getElementById w "log-text"

appendLineToLog :: Window -> String -> IO ()
appendLineToLog w msg = void $ do
    logEl <- fromJust <$> getElementById w "log-text"
    msgs <- get value logEl
    set value (msgs ++ msg ++ "\r\n") $ return logEl

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
applyClassToBody w klass = void $ getBody w # set UI.class_ klass

loadTrackImage :: Window -> OutputType -> FilePath -> IO String
loadTrackImage w outType outPath = case outType of
    PNG -> loadFile w "image/png" outPath
    SVG -> loadFile w "image/svg+xml" outPath
    _   -> error "Unsupported output format."

-- Note that the annotations are parsed in a separate step.
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
            , Pm.outputType = outType -- Why don't we read this right here?
            , Pm.xTileBounds = xBounds
            , Pm.yTileBounds = yBounds
            }

selectedAnnotations :: Window -> CartoT IO [Annotation]
selectedAnnotations w = do
    annInput <- RWS.liftIO $ selectedAnnotationsInput w
    parseAnnotations annInput

selectedFromSelect :: (Int -> a) -> String -> Window -> IO a
selectedFromSelect fSel selId = \w -> do
    optionIx <- join $ liftM (fromMaybe 0) . get UI.selection . fromJust
        <$> getElementById w selId
    return $ fSel optionIx

intToOutputType :: Int -> OutputType
intToOutputType n = case n of
        0 -> PNG
        1 -> SVG
        _ -> error "Unknown output format."

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
intToPresetRenderingParams :: Int -> Pm.RenderingParameters
intToPresetRenderingParams n = case n of
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

selectedAnnotationsInput :: Window -> IO String
selectedAnnotationsInput w = do
    join $ get value . fromJust <$> getElementById w "ann-input"

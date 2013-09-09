{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
    ( main
    ) where

import Control.Monad
import qualified Control.Monad.RWS as RWS
import Control.Monad.Error
import Control.Exception (catch, SomeException)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Directory ( doesFileExist, doesDirectoryExist
                        , getTemporaryDirectory)
import System.FilePath ((</>), takeExtension, addExtension)
import Data.Char (toUpper)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny (concatE, union, reactimate, setter)
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
    startGUI defaultConfig
        { tpPort = 10000
        , tpCustomHTML = Nothing
        , tpStatic = Just staticDir
        } setup

setup :: Window -> IO ()
setup w = void $ do

    -- Output type and tile resolution caption.
    selOutput <-
        UI.select # set UI.name "output-format-select"
            # set UI.id_ "output-format-select" #+
                [ UI.option # set UI.selected True #+ [string "PNG"]
                , UI.option #+ [string "SVG"]
                ]

    let eOutType = intToOutputType . fromMaybe (-1)
            <$> UI.selectionChange selOutput

    let ePxPtText =
            let toPxPtText x =
                    case x of
                        SVG -> "Points per tile:"
                        _   -> "Pixels per tile:" -- PNG
            in toPxPtText <$> eOutType

    strPxPtPerTile <- string "Pixels per tile:"
    bidPxPtPerTile <- BI.new (8, 128) (Pm.pixelsPerTile Pm.def, never)

    reactimate $
        void . (element strPxPtPerTile #) . set UI.text <$> ePxPtText

    -- Map bounds, grid and indices.

    let (defBMinX, defBMaxX) = Pm.xTileBounds Pm.def
        (defBMinY, defBMaxY) = Pm.yTileBounds Pm.def
        styleBoundsInput = BI.setTextInputSize 2 . BI.formatBoundsCaption (const "")

    biiBMinX <- BI.new (0, 29) (defBMinX, never) # styleBoundsInput
    biiBMaxX <- BI.new (0, 29) (defBMaxX, never) # styleBoundsInput
    biiBMinY <- BI.new (0, 29) (defBMinY, never) # styleBoundsInput
    biiBMaxY <- BI.new (0, 29) (defBMaxY, never) # styleBoundsInput

    eBoundsX <- BI.listenAsPair biiBMinX biiBMaxX
    eBoundsY <- BI.listenAsPair biiBMinY biiBMaxY

    chkDrawGrid <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-lines-chk"
            # set UI.id_ "grid-lines-chk" # set UI.checked_ True
    chkDrawIndices <-
        UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
            # set UI.id_ "grid-indices-chk" # set UI.checked_ True

    -- Preset selection and ratio field initialization.
    let presetDefAndSetter :: (Pm.RenderingParameters -> a)
                           -> Event (Pm.RenderingParameters)
                           -> (a, Event (a -> a))
        presetDefAndSetter fProj eParams =
            (fProj Pm.def, setter $ fProj <$> eParams)

        trimFracPart n = (/ 10^n) . realToFrac . truncate . (* 10^n)
        presetDefAndSetterD fProj =
            (\(x, e) -> (trimFracPart 3 x, (trimFracPart 3 .) <$> e))
                . presetDefAndSetter fProj

    selPreset <-
        UI.select # set UI.name "style-preset-select"
            # set UI.id_ "style-preset-select" #+
                [ UI.option # set UI.selected True #+ [string "Default"]
                , UI.option #+ [string "Wider track"]
                , UI.option #+ [string "Sloping ramps"]
                , UI.option #+ [string "Traditional"]
                ]

    btnPreset <- UI.button #. "button" #+ [string "Set"]

    let eSelPreset = intToPresetRenderingParams . fromMaybe (-1)
            <$> UI.selectionChange selPreset
    bPreset <- intToPresetRenderingParams 0 `stepper` eSelPreset

    let ePreset = bPreset <@ UI.click btnPreset

    -- Rendering ratios

    bidRoadW <- BI.new (0.1, 0.5) $
        presetDefAndSetterD Pm.roadWidth ePreset
    bidBridgeH <- BI.new (0, 0.5) $
        presetDefAndSetterD Pm.bridgeHeight ePreset
    bidBridgeRelW <- BI.new (1, 3) $
        presetDefAndSetterD Pm.bridgeRelativeWidth ePreset
    bidBankingRelH <- BI.new (0.25, 1) $
        presetDefAndSetterD Pm.bankingRelativeHeight ePreset

    -- Rendering parameters.
    -- Note that the annotations are parsed in a separate step.

    -- What comes below is just an unsightly way to state we listen to
    -- changes in all of the rendering parameter fields (bar the
    -- annotations one) and propagate these changes to bRenParams.
    bRenParams <- Pm.def `accumB` concatE
            [ (\x -> \p -> p {Pm.roadWidth = x})
                <$> BI.anyValueChange bidRoadW
            , (\x -> \p -> p {Pm.bridgeHeight = x})
                <$> BI.anyValueChange bidBridgeH
            , (\x -> \p -> p {Pm.bridgeRelativeWidth = x})
                <$> BI.anyValueChange bidBridgeRelW
            , (\x -> \p -> p {Pm.bankingRelativeHeight = x})
                <$> BI.anyValueChange bidBankingRelH
            , (\x -> \p -> p {Pm.pixelsPerTile = x})
                <$> BI.anyValueChange bidPxPtPerTile
            , (\x -> \p -> p {Pm.xTileBounds = ensureBoundOrder x})
                <$> eBoundsX
            , (\x -> \p -> p {Pm.yTileBounds = ensureBoundOrder x})
                <$> eBoundsY
            , (\x -> \p -> p {Pm.drawGridLines = x})
                <$> UI.checkedChange chkDrawGrid
            , (\x -> \p -> p {Pm.drawIndices = x})
                <$> UI.checkedChange chkDrawIndices
            , (\x -> \p -> p {Pm.outputType = x})
                <$> eOutType
            ]

    -- The go button.

    btnGo <- UI.button #. "go-button" #+ [string "Draw map"]
    let eGo = UI.click btnGo

    -- Log handling

    (eAppendToLog, appendToLog) <- newEvent
    (eStringToLog, stringToLog) <- newEvent

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

    (eRenderLog, renderLog) <- newEvent
    reactimate $ void . (element txaLog #) . set value . Pm.logToList
        <$> bLogContents <@ eRenderLog

    -- Misc. interesting elements

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

    txaAnns <-
        UI.textarea # set UI.name "ann-input"
            # set UI.id_ "ann-input"
            # set UI.cols "25" # set UI.rows "5"

    imgMap <- UI.img # set UI.id_ "track-map" # set UI.src "static/images/welcome.png"

    -- Assembling the interface HTML.

    return w # set title "Stunts Cartography - Track Viewer"
    UI.addStyleSheet w "viewer.css"

    theBody <- getBody w # set UI.id_ "the-body" #. "blank-horizon" #+
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
                , BI.toElement bidRoadW
                ]
            , UI.p #+
                [ string "Bridge height:", UI.br
                , BI.toElement bidBridgeH
                ]
            , UI.p #+
                [ string "Bridge relative width:", UI.br
                , BI.toElement bidBridgeRelW
                ]
            , UI.p #+
                [ string "Banking relative height:", UI.br
                , BI.toElement bidBankingRelH
                ]
            , UI.p #+
                [ element strPxPtPerTile, UI.br
                , BI.toElement bidPxPtPerTile
                ]
            , UI.p #+
                [ string "Grid?", element chkDrawGrid
                , string " Indices?", element chkDrawIndices
                ]
            , UI.p #+
                [ string "Map bounds (0 - 29):", UI.br
                , string "x from ", BI.toElement biiBMinX
                , string " to ", BI.toElement biiBMaxX, UI.br
                , string "y from ", BI.toElement biiBMinY
                , string " to ", BI.toElement biiBMaxY
                ]
            , UI.p #+
                [ string "Annotations - "
                , UI.a # set UI.text "help"
                    # set UI.href ("static/annotations-help.html")
                    # set UI.target "_blank"
                , UI.br
                , element txaAnns
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
                     -> IO (Pm.RenderingState, Pm.RenderingLog)
        runRenderMap params st = do

            trkRelPath <- itxTrkPath # get value
            basePath <- itxBasePath # get value
            let trkPath = basePath </> trkRelPath

            outcome <- runErrorT $ do

                trkExists <- liftIO $ doesFileExist trkPath
                unless trkExists . void $
                    throwError "File does not exist."

                let fileExt = map toUpper $ takeExtension trkPath
                    extIsKnown = fileExt == ".TRK" || fileExt == ".RPL"
                unless extIsKnown . void $
                    throwError "Bad file extension (should be .TRK or .RPL, in upper or lower case)."

                mFileSize <- liftIO $ retrieveFileSize trkPath
                let sizeIsCorrect = mFileSize == Just 1802
                    badTRKSize = fileExt == ".TRK" && not sizeIsCorrect
                when badTRKSize . void $
                    throwError "Bad file size (.TRK files must have 1802 bytes)."

                -- Decide on input format.
                let pngWriter :: FilePath -> CartoT (ErrorT String IO) Pm.PostRenderInfo
                    pngWriter = case fileExt of
                        ".TRK" -> writePngFromTrk
                        ".RPL" -> writePngFromRpl
                        _      -> error "Unrecognized input extension."

                -- Parse annotations and render the map.
                let goCarto :: CartoT (ErrorT String IO) Pm.PostRenderInfo
                    goCarto = do
                        anns <- liftIO (txaAnns # get value)
                            >>=  parseAnnotations
                        RWS.local (\p -> p{ Pm.annotationSpecs = anns}) $
                            pngWriter trkPath
                (postRender,st',logW) <- RWS.runRWST goCarto params st

                -- Update the UI.
                liftIO $ do
                    element theBody #. horizonClass (Pm.renderedTrackHorizon postRender)
                    let outType = Pm.outputType params
                    trackImage <- loadTrackImage w outType $ Pm.outputPath postRender
                    trkUri <- loadTmpTrk w postRender
                    terrainUri <- loadTmpTerrainTrk w postRender
                    element imgMap # set UI.src trackImage
                    element lnkTrk # set UI.href trkUri
                    element lnkTerrTrk # set UI.href terrainUri

                    return (st', logW)

                `catchError` \errorMsg -> do

                    liftIO $ do
                        element theBody #. "blank-horizon"
                        element imgMap # set UI.src "static/images/failure.png"
                        runFunction w $ unsetSaveLinksHref

                    throwError errorMsg

            return $ either ((,) st . Pm.logFromList) id outcome


    -- Collecting the parameters and firing the main action.

    mdo
        let eRenParams = bRenParams <@ eGo

        -- Output from the main action, input for the next run.
        (eRenState, fireRenState) <- newEvent
        bRenState <- Pm.def `stepper` eRenState

        let (eRenParamsDiffEStyle, eRenParamsSameEStyle) = split $
                (\es -> \p -> if Pm.toElemStyle p /= es
                    then Left p else Right p) <$> bRenEStyle <@> eRenParams

            -- The immediate trigger of the main action.
            eParamsAndStateAfterEStyleCheck =
                (flip (,) . Pm.clearElementCache <$> bRenState
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
        reactimate $
            (\(p, st) -> runRenderMap p st
                >>= \(st', w) -> fireRenState st'
                    >> appendToLog w >> renderLog ())
                        <$> eParamsAndStateAfterEStyleCheck


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

horizonClass :: Horizon -> String
horizonClass horizon = case horizon of
    Desert   -> "desert-horizon"
    Alpine   -> "alpine-horizon"
    City     -> "city-horizon"
    Country  -> "country-horizon"
    Tropical -> "tropical-horizon"
    _        -> "unknown-horizon"

loadTrackImage :: Window -> OutputType -> FilePath -> IO String
loadTrackImage w outType outPath = case outType of
    PNG -> loadFile w "image/png" outPath
    SVG -> loadFile w "image/svg+xml" outPath
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

ensureBoundOrder :: (Int, Int) -> (Int, Int)
ensureBoundOrder bounds@(z, w) = if z > w then (w, z) else bounds

-- Reading of fields without reactive-banana. Currently unused.
{-
selectedFromSelect :: (Int -> a) -> String -> Window -> IO a
selectedFromSelect fSel selId = \w -> do
    optionIx <- join $ liftM (fromMaybe 0) . get UI.selection . fromJust
        <$> getElementById w selId
    return $ fSel optionIx


selectedNumFromTextInput :: (Num a, Read a, Ord a)
                         => String -> a -> a -> a
                         -> Window -> IO a
selectedNumFromTextInput elemId minVal defVal maxVal = \w -> do
    inputStr <- join $ get value . fromJust
        <$> getElementById w elemId
    let val = fromMaybe defVal . readMaybe $ inputStr
    return $ min maxVal . max minVal $ val

selectedBoolFromCheckbox :: String -> Window -> IO Bool
selectedBoolFromCheckbox elemId = \w ->
    join $ get UI.checked . fromJust <$> getElementById w elemId
-}

-- Other unused utility functions we do not want to throw away (yet).
{-
setTrackMapVisibility :: Bool -> JSFunction ()
setTrackMapVisibility visible
    | visible   = ffi "document.getElementById('track-map').style.display='block';"
    | otherwise = ffi "document.getElementById('track-map').style.display='none';"
-}

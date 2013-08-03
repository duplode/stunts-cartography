module Main where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Diagrams.Backend.Cairo (OutputType(..))

import Output
import Track (Horizon(..))
import qualified Parameters as Pm

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
            , UI.p #+ [string ".TRK relative path:"]
            , UI.input # set UI.type_ "text" # set UI.name "trk-input"
                # set UI.id_ "trk-input"
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
                ]
            , UI.p #+
                [ string "Grid indices?"
                , UI.input # set UI.type_ "checkbox" # set UI.name "grid-indices-chk"
                    # set UI.id_ "grid-indices-chk" # set UI.checked_ True
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
    roadW <- selectedRoadWidth w
    bridgeH <- selectedBridgeHeight w
    bridgeRelW <- selectedBridgeRelativeWidth w
    bankRelH <- selectedBankingRelativeHeight w
    pxPerTile <- selectedPixelsPerTile w
    drawGrid <- selectedDrawGridLines w
    drawIxs <- selectedDrawGridIndices w
    let params = Pm.defaultRenderingParameters
            { Pm.roadWidth = roadW
            , Pm.bridgeHeight = bridgeH
            , Pm.bridgeRelativeWidth = bridgeRelW
            , Pm.bankingRelativeHeight = bankRelH
            , Pm.pixelsPerTile = pxPerTile
            , Pm.drawGridLines = drawGrid
            , Pm.drawIndices = drawIxs
            , Pm.outputType = outType
            }
    trkExists <- doesFileExist trkPath
    mFileSize <- retrieveFileSize trkPath
    let sizeIsCorrect = mFileSize == Just 1802
        proceedWithLoading = trkExists && sizeIsCorrect
    if proceedWithLoading
        then do
            postRender <- writePngOutput params trkPath
            applyHorizonClass (Pm.renderedTrackHorizon postRender) w
            trackImage <- loadTrackImage outType w
            (fromJust <$> getElementById w "track-map") # set UI.src trackImage
        else do
            runFunction w $ applyClassToBody "blank-horizon"
            (fromJust <$> getElementById w "track-map") # set UI.src ""
    return ()

applyHorizonClass :: Horizon -> Window -> IO ()
applyHorizonClass horizon = \w -> do
    let horizonClass = case horizon of
            Desert   -> "desert-horizon"
            Alpine   -> "alpine-horizon"
            City     -> "city-horizon"
            Country  -> "country-horizon"
            Tropical -> "tropical-horizon"
            _        -> "unknown-horizon"
    runFunction w $ applyClassToBody horizonClass

applyClassToBody :: String -> JSFunction ()
applyClassToBody = ffi "document.body.className = %1;"

loadTrackImage :: OutputType -> Window -> IO String
loadTrackImage outType w = case outType of
    PNG -> loadFile w "image/png" "./stunts-cartography-map-tmp.png"
    SVG -> loadFile w "image/svg+xml" "./stunts-cartography-map-tmp.svg"
    _   -> loadFile w "image/png" "./stunts-cartography-map" --Nonsense

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

--Lifted from RWH chapter 9.
retrieveFileSize :: FilePath -> IO (Maybe Integer)
retrieveFileSize path = handle nothingHandler $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)
    where
    nothingHandler :: IOError -> IO (Maybe Integer)
    nothingHandler = \_ -> return Nothing


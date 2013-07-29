module Main where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromJust, fromMaybe)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import Control.Exception (handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Output
import qualified Parameters as Params

main :: IO ()
main = do
    startGUI Config
        { tpPort = 10000
        , tpCustomHTML = Nothing
        , tpStatic = "."
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Yet Another Track Viewer"
    UI.addStyleSheet w "viewer.css"
    trackPng <- loadTrackPng w
    getBody w #+
        [ UI.div #. "left-bar" #+
            [ UI.p #+ [string ".TRK file path:"]
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
            , UI.p #+ [string "Pixels per tile (8 - 64):"]
            , UI.input # set UI.type_ "text" # set UI.name "px-per-tile-input"
                # set UI.id_ "px-per-tile-input" # set value "32"
            , mkButtonGo
            ]
        , UI.div #. "main-wrap" #+
            [ UI.img # set UI.id_ "track-map"]
        ]

mkButtonGo :: IO Element
mkButtonGo = do
    button <- UI.button #. "button" #+ [string "Go!"]
    on UI.click button $ \_ -> do
        w <- fromJust <$> getWindow button --TODO: ick
        trkPath <- join $ get value . fromJust
            <$> getElementById w "trk-input"
        roadW <- selectedRoadWidth w
        bridgeH <- selectedBridgeHeight w
        bridgeRelW <- selectedBridgeRelativeWidth w
        pxPerTile <- selectedPixelsPerTile w
        let params = Params.defaultRenderingParameters
                { Params.roadWidth = roadW
                , Params.bridgeHeight = bridgeH
                , Params.bridgeRelativeWidth = bridgeRelW
                , Params.pixelsPerTile = pxPerTile
                }
        trkExists <- doesFileExist trkPath
        mFileSize <- retrieveFileSize trkPath
        let sizeIsCorrect = mFileSize == Just 1802
        when (trkExists && sizeIsCorrect) $ writePngOutput params trkPath
        trackPng <- loadTrackPng w
        (fromJust <$> getElementById w "track-map") # set UI.src trackPng
        --getBody w #+ [UI.p #. "message" #+ [string trkPath]]
    return button

loadTrackPng :: Window -> IO String
loadTrackPng w = loadFile w "image/png" "./test.png"

selectedDoubleFromTextInput :: String -> Double -> Double -> Double
                            -> Window -> IO Double
selectedDoubleFromTextInput elemId minVal defVal maxVal = \w -> do
    inputStr <- join $ get value . fromJust
        <$> getElementById w elemId
    let val = fromMaybe defVal . readMaybe $ inputStr
    return $ min maxVal . max minVal $ val

selectedRoadWidth :: Window -> IO Double
selectedRoadWidth =
    selectedDoubleFromTextInput "road-w-input" 0.1 0.2 0.5

selectedBridgeHeight :: Window -> IO Double
selectedBridgeHeight = do
    selectedDoubleFromTextInput "bridge-h-input" 0 0 0.5

selectedBridgeRelativeWidth :: Window -> IO Double
selectedBridgeRelativeWidth = do
    selectedDoubleFromTextInput "bridge-rel-w-input" 1 2 3

selectedPixelsPerTile :: Window -> IO Double
selectedPixelsPerTile =
    selectedDoubleFromTextInput "px-per-tile-input" 8 32 64


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

--Test snippets
--Radio buttons for road width selection.
            {-
            , UI.p #+
                [ UI.input # set UI.type_ "radio" # set UI.name "road-w"
                    # set UI.value "standard" # set UI.id_ "road-w-standard"
                    # set UI.checked True
                    # registerCopyValueOnChange "road-w-copy"
                , string "Standard (1/5)"
                ]
            , UI.p #+
                [ UI.input # set UI.type_ "radio" # set UI.name "road-w"
                    # set UI.value "wider" # set UI.id_ "road-w-wider"
                    # registerCopyValueOnChange "road-w-copy"
                , string "Wider (1/4)"
                ]
            , UI.p #+
                [ UI.input # set UI.type_ "radio" # set UI.name "road-w"
                    # set UI.value "wide" # set UI.id_ "road-w-wide"
                    # registerCopyValueOnChange "road-w-copy"
                , string "Wide (1/3)"
                ]
            , UI.input # set UI.type_ "hidden" # set UI.name "road-w-copy"
                {- # set UI.value "standard" -} # set UI.id_ "road-w-copy"
            -}
--(Failed) attempts to make use of the radiobuttons under Threepenny 0.1.0.1.
{-
--There are no checks on whether the element exists.
setValueForSomeId :: Window -> String -> String -> IO Element
setValueForSomeId w val eid =
    (fromJust <$> getElementById w eid) # set value val

--There are no checks on whether the element has a window.
registerCopyValueOnChange :: String -> IO Element -> IO Element
registerCopyValueOnChange destId el = do
    el' <- el
    on UI.click el' $ \_ -> do
        w <- fromJust <$> getWindow el'
        return w # set title "FOO!"
        val <- get value el'
        setValueForSomeId w val destId
    el

selectedRoadWidth :: Window -> IO Double
selectedRoadWidth w = do
    widthStr <- join $ get value . fromJust
        <$> getElementById w "road-w-copy"
    return $ case widthStr of
        "standard" -> 1 / 5
        "wider"    -> 1 / 4
        "wide"     -> 1 / 3
        _          -> 1 / 5
-}


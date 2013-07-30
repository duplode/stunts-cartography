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
import Diagrams.Backend.Cairo (OutputType(..))

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
    return w # set title "Stunts Cartography - Track Viewer"
    UI.addStyleSheet w "viewer.css"
    getBody w #+
        [ UI.div #. "left-bar" #+
            [ UI.p #+ [string "Generate image:"]
            , mkButtonGo
            , mkButtonSVG
            , UI.p #+ [string ".TRK file path:"]
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
            , UI.p #+ [string "Grid mode:"]
            , UI.ul #+
                [ UI.li #+ [string "0: None"]
                , UI.li #+ [string "1: Lines"]
                , UI.li #+ [string "2: Indices"]
                , UI.li #+ [string "3: Both"]
                ]
            , UI.input # set UI.type_ "text" # set UI.name "grid-mode-input"
                # set UI.id_ "grid-mode-input" # set value "3"
            ]
        , UI.div #. "main-wrap" #+
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
    trkPath <- join $ get value . fromJust
        <$> getElementById w "trk-input"
    roadW <- selectedRoadWidth w
    bridgeH <- selectedBridgeHeight w
    bridgeRelW <- selectedBridgeRelativeWidth w
    bankRelH <- selectedBankingRelativeHeight w
    pxPerTile <- selectedPixelsPerTile w
    (drawGrid, drawIxs) <- parseGridMode
        <$> selectedGridMode w
    let params = Params.defaultRenderingParameters
            { Params.roadWidth = roadW
            , Params.bridgeHeight = bridgeH
            , Params.bridgeRelativeWidth = bridgeRelW
            , Params.bankingRelativeHeight = bankRelH
            , Params.pixelsPerTile = pxPerTile
            , Params.drawGridLines = drawGrid
            , Params.drawIndices = drawIxs
            , Params.outputType = outType
            }
    trkExists <- doesFileExist trkPath
    mFileSize <- retrieveFileSize trkPath
    let sizeIsCorrect = mFileSize == Just 1802
        proceedWithLoading = trkExists && sizeIsCorrect
    when proceedWithLoading $ writePngOutput params trkPath
    trackImage <- loadTrackImage outType w
    (fromJust <$> getElementById w "track-map")
        # set UI.src (if proceedWithLoading then trackImage else "")
    --getBody w #+ [UI.p #. "message" #+ [string trkPath]]
    return ()

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

selectedGridMode :: Window -> IO Int
selectedGridMode = do
    selectedNumFromTextInput "grid-mode-input" 0 3 3

--fst: grid lines; snd: indices.
--TODO: Possibly refactor as a bit field.
parseGridMode :: Int -> (Bool, Bool)
parseGridMode n = case n of
    0 -> (False, False)
    1 -> (True, False)
    2 -> (False, True)
    3 -> (True, True)
    _ -> (True, True)

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

